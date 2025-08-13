(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u300))
(define-constant err-escrow-not-found (err u301))
(define-constant err-invalid-state (err u302))
(define-constant err-expired (err u303))
(define-constant err-not-expired (err u304))
(define-constant err-invalid-amount (err u305))
(define-constant err-transfer-failed (err u306))
(define-constant err-already-exists (err u307))

(define-constant escrow-period-blocks u1440)
(define-constant dispute-period-blocks u720)

(define-map escrows
    { token-id: uint, buyer: principal }
    {
        seller: principal,
        amount: uint,
        deposit-block: uint,
        state: (string-ascii 16)
    }
)

(define-map disputes
    { token-id: uint, buyer: principal }
    {
        reason: (string-utf8 256),
        dispute-block: uint,
        resolved: bool,
        resolution: (string-ascii 16)
    }
)

(define-data-var total-escrows uint u0)
(define-data-var total-disputes uint u0)

(define-public (create-escrow (token-id uint) (seller principal) (amount uint))
    (let ((escrow-key { token-id: token-id, buyer: tx-sender }))
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (is-none (map-get? escrows escrow-key)) err-already-exists)
        
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        (map-set escrows escrow-key
            {
                seller: seller,
                amount: amount,
                deposit-block: stacks-block-height,
                state: "active"
            }
        )
        
        (var-set total-escrows (+ (var-get total-escrows) u1))
        (ok true)
    )
)

(define-public (complete-escrow (token-id uint) (buyer principal))
    (let ((escrow-key { token-id: token-id, buyer: buyer })
          (escrow-data (unwrap! (map-get? escrows escrow-key) err-escrow-not-found)))
        (asserts! (is-eq tx-sender buyer) err-not-authorized)
        (asserts! (is-eq (get state escrow-data) "active") err-invalid-state)
        
        (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender (get seller escrow-data))))
        
        (map-set escrows escrow-key
            (merge escrow-data { state: "completed" })
        )
        
        (ok true)
    )
)

(define-public (raise-dispute (token-id uint) (reason (string-utf8 256)))
    (let ((escrow-key { token-id: token-id, buyer: tx-sender })
          (escrow-data (unwrap! (map-get? escrows escrow-key) err-escrow-not-found))
          (dispute-key { token-id: token-id, buyer: tx-sender }))
        (asserts! (is-eq (get state escrow-data) "active") err-invalid-state)
        (asserts! (< (- stacks-block-height (get deposit-block escrow-data)) escrow-period-blocks) err-expired)
        
        (map-set disputes dispute-key
            {
                reason: reason,
                dispute-block: stacks-block-height,
                resolved: false,
                resolution: ""
            }
        )
        
        (map-set escrows escrow-key
            (merge escrow-data { state: "disputed" })
        )
        
        (var-set total-disputes (+ (var-get total-disputes) u1))
        (ok true)
    )
)

(define-public (resolve-dispute (token-id uint) (buyer principal) (resolution (string-ascii 16)))
    (let ((escrow-key { token-id: token-id, buyer: buyer })
          (dispute-key { token-id: token-id, buyer: buyer })
          (escrow-data (unwrap! (map-get? escrows escrow-key) err-escrow-not-found))
          (dispute-data (unwrap! (map-get? disputes dispute-key) err-escrow-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (is-eq (get state escrow-data) "disputed") err-invalid-state)
        (asserts! (not (get resolved dispute-data)) err-invalid-state)
        
        (if (is-eq resolution "buyer")
            (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender buyer)))
            (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender (get seller escrow-data))))
        )
        
        (map-set disputes dispute-key
            (merge dispute-data { resolved: true, resolution: resolution })
        )
        
        (map-set escrows escrow-key
            (merge escrow-data { state: "resolved" })
        )
        
        (ok true)
    )
)

(define-public (cancel-expired-escrow (token-id uint) (buyer principal))
    (let ((escrow-key { token-id: token-id, buyer: buyer })
          (escrow-data (unwrap! (map-get? escrows escrow-key) err-escrow-not-found)))
        (asserts! (is-eq (get state escrow-data) "active") err-invalid-state)
        (asserts! (>= (- stacks-block-height (get deposit-block escrow-data)) escrow-period-blocks) err-not-expired)
        
        (try! (as-contract (stx-transfer? (get amount escrow-data) tx-sender buyer)))
        
        (map-set escrows escrow-key
            (merge escrow-data { state: "cancelled" })
        )
        
        (ok true)
    )
)

(define-read-only (get-escrow-info (token-id uint) (buyer principal))
    (map-get? escrows { token-id: token-id, buyer: buyer })
)

(define-read-only (get-dispute-info (token-id uint) (buyer principal))
    (map-get? disputes { token-id: token-id, buyer: buyer })
)

(define-read-only (is-escrow-active (token-id uint) (buyer principal))
    (match (map-get? escrows { token-id: token-id, buyer: buyer })
        escrow-data (is-eq (get state escrow-data) "active")
        false
    )
)

(define-read-only (is-escrow-expired (token-id uint) (buyer principal))
    (match (map-get? escrows { token-id: token-id, buyer: buyer })
        escrow-data (and 
            (is-eq (get state escrow-data) "active")
            (>= (- stacks-block-height (get deposit-block escrow-data)) escrow-period-blocks)
        )
        false
    )
)

(define-read-only (get-escrow-time-remaining (token-id uint) (buyer principal))
    (match (map-get? escrows { token-id: token-id, buyer: buyer })
        escrow-data
        (let ((elapsed-blocks (- stacks-block-height (get deposit-block escrow-data))))
            (if (< elapsed-blocks escrow-period-blocks)
                (some (- escrow-period-blocks elapsed-blocks))
                (some u0)
            )
        )
        none
    )
)

(define-read-only (get-escrow-stats)
    {
        total-escrows: (var-get total-escrows),
        total-disputes: (var-get total-disputes)
    }
)
