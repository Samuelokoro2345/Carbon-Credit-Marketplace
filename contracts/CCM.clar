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
