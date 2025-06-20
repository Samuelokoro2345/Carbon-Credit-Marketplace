;; (impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

(define-non-fungible-token carbon-credit uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-listing-not-found (err u102))
(define-constant err-invalid-price (err u103))
(define-constant err-already-listed (err u104))

(define-data-var next-token-id uint u1)
(define-data-var total-credits uint u0)

(define-map token-metadata 
    uint 
    {
        credit-amount: uint,
        verification-date: uint,
        project-id: (string-ascii 32),
        location: (string-ascii 32)
    }
)

(define-map market
    uint
    {
        price: uint,
        seller: principal
    }
)

(define-public (mint (credit-amount uint) 
                    (project-id (string-ascii 32))
                    (location (string-ascii 32)))
    (let ((token-id (var-get next-token-id)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (try! (nft-mint? carbon-credit token-id tx-sender))
        (map-set token-metadata
            token-id
            {
                credit-amount: credit-amount,
                verification-date: stacks-block-height,
                project-id: project-id,
                location: location
            }
        )
        (var-set next-token-id (+ token-id u1))
        (var-set total-credits (+ (var-get total-credits) credit-amount))
        (ok token-id)
    )
)

(define-public (list-credit (token-id uint) (price uint))
    (let ((owner (unwrap! (nft-get-owner? carbon-credit token-id) err-listing-not-found)))
        (asserts! (is-eq tx-sender owner) err-not-token-owner)
        (asserts! (> price u0) err-invalid-price)
        (asserts! (is-none (map-get? market token-id)) err-already-listed)
        (map-set market token-id {price: price, seller: tx-sender})
        (ok true)
    )
)

(define-public (unlist-credit (token-id uint))
    (let ((listing (unwrap! (map-get? market token-id) err-listing-not-found)))
        (asserts! (is-eq tx-sender (get seller listing)) err-not-token-owner)
        (map-delete market token-id)
        (ok true)
    )
)

(define-public (purchase-credit (token-id uint))
    (let (
        (listing (unwrap! (map-get? market token-id) err-listing-not-found))
        (price (get price listing))
        (seller (get seller listing))
    )
        (try! (stx-transfer? price tx-sender seller))
        (try! (nft-transfer? carbon-credit token-id seller tx-sender))
        (map-delete market token-id)
        (ok true)
    )
)

(define-read-only (get-token-metadata (token-id uint))
    (map-get? token-metadata token-id)
)

(define-read-only (get-listing (token-id uint))
    (map-get? market token-id)
)

(define-read-only (get-total-credits)
    (var-get total-credits)
)

(define-read-only (get-last-token-id)
    (- (var-get next-token-id) u1)
)


(define-constant err-invalid-token-ids (err u105))
(define-constant err-empty-list (err u106))

(define-public (batch-mint (credit-amounts (list 20 uint)) 
                          (project-ids (list 20 (string-ascii 32)))
                          (locations (list 20 (string-ascii 32))))
    (let ((list-length (len credit-amounts)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> list-length u0) err-empty-list)
        (asserts! (and (is-eq list-length (len project-ids)) (is-eq list-length (len locations))) err-invalid-token-ids)
        (ok (map batch-mint-helper credit-amounts project-ids locations))
    )
)

(define-private (batch-mint-helper (credit-amount uint) (project-id (string-ascii 32)) (location (string-ascii 32)))
    (let ((token-id (var-get next-token-id)))
        (match (nft-mint? carbon-credit token-id tx-sender)
            success
            (begin
                (map-set token-metadata
                    token-id
                    {
                        credit-amount: credit-amount,
                        verification-date: stacks-block-height,
                        project-id: project-id,
                        location: location
                    }
                )
                (var-set next-token-id (+ token-id u1))
                (var-set total-credits (+ (var-get total-credits) credit-amount))
                (ok token-id))
            error (err u0))
    )
)

(define-public (batch-list-credits (token-ids (list 20 uint)) (prices (list 20 uint)))
    (let ((list-length (len token-ids)))
        (asserts! (> list-length u0) err-empty-list)
        (asserts! (is-eq list-length (len prices)) err-invalid-token-ids)
        (ok (map batch-list-helper token-ids prices))
    )
)

(define-private (batch-list-helper (token-id uint) (price uint))
    (let ((owner (unwrap! (nft-get-owner? carbon-credit token-id) err-listing-not-found)))
        (if (and 
                (is-eq tx-sender owner)
                (> price u0)
                (is-none (map-get? market token-id)))
            (ok (begin
                (map-set market token-id {price: price, seller: tx-sender})
                true))
            (err u0))
    )
)

(define-public (batch-purchase-credits (token-ids (list 20 uint)))
    (let ((list-length (len token-ids)))
        (asserts! (> list-length u0) err-empty-list)
        (ok (map batch-purchase-helper token-ids))
    )
)

(define-private (batch-purchase-helper (token-id uint))
    (let (
        (listing (unwrap! (map-get? market token-id) err-listing-not-found))
        (price (get price listing))
        (seller (get seller listing))
    )
        (if (is-ok (stx-transfer? price tx-sender seller))
            (if (is-ok (nft-transfer? carbon-credit token-id seller tx-sender))
                (begin
                    (map-delete market token-id)
                    (ok true))
                (err u0))
            (err u0))
    )
)


(define-constant err-already-retired (err u107))
(define-constant retirement-address 'SP000000000000000000002Q6VF78)

(define-map retired-credits
    uint
    {
        retired-by: principal,
        retirement-date: uint,
        retirement-reason: (string-utf8 256)
    }
)

(define-data-var total-retired-credits uint u0)

(define-public (retire-credit (token-id uint) (retirement-reason (string-utf8 256)))
    (let ((owner (unwrap! (nft-get-owner? carbon-credit token-id) err-listing-not-found))
          (metadata (unwrap! (map-get? token-metadata token-id) err-listing-not-found)))
        (asserts! (is-eq tx-sender owner) err-not-token-owner)
        (asserts! (is-none (map-get? retired-credits token-id)) err-already-retired)
        
        ;; Transfer to retirement address (effectively burning it)
        (try! (nft-transfer? carbon-credit token-id tx-sender retirement-address))
        
        ;; Record retirement details
        (map-set retired-credits token-id 
            {
                retired-by: tx-sender,
                retirement-date: stacks-block-height,
                retirement-reason: retirement-reason
            }
        )
        
        ;; If listed, remove from market
        (if (is-some (map-get? market token-id))
            (map-delete market token-id)
            true)
            
        ;; Update total retired credits
        (var-set total-retired-credits (+ (var-get total-retired-credits) (get credit-amount metadata)))
        
        (ok true)
    )
)

(define-read-only (get-retirement-info (token-id uint))
    (map-get? retired-credits token-id)
)

(define-read-only (is-retired (token-id uint))
    (is-some (map-get? retired-credits token-id))
)

(define-read-only (get-total-retired-credits)
    (var-get total-retired-credits)
)

