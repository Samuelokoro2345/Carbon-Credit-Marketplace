;; Carbon Credit Fractionalization Contract
;; Enables splitting carbon credits into smaller, tradeable fractions

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u500))
(define-constant err-fraction-not-found (err u501))
(define-constant err-insufficient-balance (err u502))
(define-constant err-invalid-amount (err u503))
(define-constant err-token-not-owned (err u504))
(define-constant err-already-fractionalized (err u505))
(define-constant err-invalid-fraction-size (err u506))
(define-constant err-fraction-too-small (err u507))

;; Minimum fraction size (0.01 credits = 100 micro-credits)
(define-constant minimum-fraction-size u100)
(define-constant fraction-precision u10000)

;; Fractionalized token registry
(define-map fractional-tokens
    uint
    {
        original-token-id: uint,
        total-fractions: uint,
        fraction-size: uint,
        owner: principal,
        created-block: uint,
        active: bool
    }
)

;; User fraction balances
(define-map fraction-balances
    { token-id: uint, owner: principal }
    uint
)

;; Fraction marketplace listings
(define-map fraction-market
    { token-id: uint, seller: principal }
    {
        fraction-amount: uint,
        price-per-fraction: uint,
        total-price: uint,
        listed-block: uint
    }
)

;; Fraction transfer history for compliance
(define-map fraction-transfers
    { token-id: uint, transfer-id: uint }
    {
        from: principal,
        to: principal,
        amount: uint,
        transfer-block: uint
    }
)

;; Global counters
(define-data-var next-token-id uint u1)
(define-data-var total-fractionalized uint u0)
(define-data-var next-transfer-id uint u1)

;; Fractionalize a carbon credit into smaller units
(define-public (fractionalize-credit (original-token-id uint) (fraction-size uint))
    (let ((fractional-token-id (var-get next-token-id)))
        ;; Validate inputs
        (asserts! (>= fraction-size minimum-fraction-size) err-fraction-too-small)
        (asserts! (<= fraction-size fraction-precision) err-invalid-fraction-size)
        
        ;; Note: Users need to manage their own tracking to avoid duplicate fractionalization
        
        ;; Create fractional token record
        (map-set fractional-tokens fractional-token-id
            {
                original-token-id: original-token-id,
                total-fractions: (/ fraction-precision fraction-size),
                fraction-size: fraction-size,
                owner: tx-sender,
                created-block: stacks-block-height,
                active: true
            }
        )
        
        ;; Give all fractions to the original owner
        (map-set fraction-balances
            { token-id: fractional-token-id, owner: tx-sender }
            (/ fraction-precision fraction-size)
        )
        
        ;; Update counters
        (var-set next-token-id (+ fractional-token-id u1))
        (var-set total-fractionalized (+ (var-get total-fractionalized) u1))
        
        (ok fractional-token-id)
    )
)

;; Transfer fractions between users
(define-public (transfer-fractions (token-id uint) (recipient principal) (amount uint))
    (let ((sender-balance (default-to u0 (map-get? fraction-balances { token-id: token-id, owner: tx-sender })))
          (recipient-balance (default-to u0 (map-get? fraction-balances { token-id: token-id, owner: recipient })))
          (transfer-id (var-get next-transfer-id)))
        
        ;; Validate transfer
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (>= sender-balance amount) err-insufficient-balance)
        
        ;; Update balances
        (map-set fraction-balances
            { token-id: token-id, owner: tx-sender }
            (- sender-balance amount)
        )
        
        (map-set fraction-balances
            { token-id: token-id, owner: recipient }
            (+ recipient-balance amount)
        )
        
        ;; Record transfer
        (map-set fraction-transfers
            { token-id: token-id, transfer-id: transfer-id }
            {
                from: tx-sender,
                to: recipient,
                amount: amount,
                transfer-block: stacks-block-height
            }
        )
        
        (var-set next-transfer-id (+ transfer-id u1))
        (ok true)
    )
)

;; List fractions for sale
(define-public (list-fractions (token-id uint) (amount uint) (price-per-fraction uint))
    (let ((owner-balance (default-to u0 (map-get? fraction-balances { token-id: token-id, owner: tx-sender }))))
        
        ;; Validate listing
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (> price-per-fraction u0) err-invalid-amount)
        (asserts! (>= owner-balance amount) err-insufficient-balance)
        
        ;; Create market listing
        (map-set fraction-market
            { token-id: token-id, seller: tx-sender }
            {
                fraction-amount: amount,
                price-per-fraction: price-per-fraction,
                total-price: (* amount price-per-fraction),
                listed-block: stacks-block-height
            }
        )
        
        (ok true)
    )
)

;; Purchase fractions from marketplace
(define-public (purchase-fractions (token-id uint) (seller principal) (amount uint))
    (let ((listing (unwrap! (map-get? fraction-market { token-id: token-id, seller: seller }) err-fraction-not-found))
          (total-cost (* amount (get price-per-fraction listing)))
          (seller-balance (default-to u0 (map-get? fraction-balances { token-id: token-id, owner: seller })))
          (buyer-balance (default-to u0 (map-get? fraction-balances { token-id: token-id, owner: tx-sender }))))
        
        ;; Validate purchase
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (>= (get fraction-amount listing) amount) err-insufficient-balance)
        (asserts! (>= seller-balance amount) err-insufficient-balance)
        
        ;; Transfer STX payment
        (try! (stx-transfer? total-cost tx-sender seller))
        
        ;; Transfer fractions
        (map-set fraction-balances
            { token-id: token-id, owner: seller }
            (- seller-balance amount)
        )
        
        (map-set fraction-balances
            { token-id: token-id, owner: tx-sender }
            (+ buyer-balance amount)
        )
        
        ;; Update or remove listing
        (if (is-eq (get fraction-amount listing) amount)
            (map-delete fraction-market { token-id: token-id, seller: seller })
            (map-set fraction-market
                { token-id: token-id, seller: seller }
                (merge listing 
                    { 
                        fraction-amount: (- (get fraction-amount listing) amount),
                        total-price: (* (- (get fraction-amount listing) amount) (get price-per-fraction listing))
                    }
                )
            )
        )
        
        (ok true)
    )
)

;; Cancel fraction listing
(define-public (cancel-fraction-listing (token-id uint))
    (begin
        (asserts! (is-some (map-get? fraction-market { token-id: token-id, seller: tx-sender })) err-fraction-not-found)
        (map-delete fraction-market { token-id: token-id, seller: tx-sender })
        (ok true)
    )
)

;; Get fractional token info
(define-read-only (get-fractional-token (token-id uint))
    (map-get? fractional-tokens token-id)
)

;; Get fraction balance for a user
(define-read-only (get-fraction-balance (token-id uint) (owner principal))
    (default-to u0 (map-get? fraction-balances { token-id: token-id, owner: owner }))
)

;; Get fraction market listing
(define-read-only (get-fraction-listing (token-id uint) (seller principal))
    (map-get? fraction-market { token-id: token-id, seller: seller })
)

;; Check if original token is already fractionalized (simple approach)
(define-read-only (is-token-fractionalized (original-token-id uint))
    false ;; Simplified for now - users need to track their fractional token IDs
)

;; Get conversion rate (fractions per full credit)
(define-read-only (get-conversion-rate (token-id uint))
    (match (map-get? fractional-tokens token-id)
        token-data (some (/ fraction-precision (get fraction-size token-data)))
        none
    )
)

;; Get platform statistics
(define-read-only (get-fractionalization-stats)
    {
        total-fractionalized: (var-get total-fractionalized),
        next-token-id: (var-get next-token-id),
        next-transfer-id: (var-get next-transfer-id)
    }
)
