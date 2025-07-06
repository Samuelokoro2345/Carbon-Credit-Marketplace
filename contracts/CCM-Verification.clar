(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u200))
(define-constant err-already-verified (err u201))
(define-constant err-token-not-found (err u202))
(define-constant err-verifier-not-found (err u203))
(define-constant err-invalid-score (err u204))

(define-map authorized-verifiers
    principal
    {
        name: (string-ascii 64),
        certification: (string-ascii 128),
        active: bool,
        verification-count: uint
    }
)

(define-map credit-verifications
    uint
    {
        verifier: principal,
        verification-date: uint,
        trust-score: uint,
        verification-notes: (string-utf8 512),
        methodology-used: (string-ascii 64),
        verified: bool
    }
)

(define-data-var total-verifiers uint u0)
(define-data-var total-verifications uint u0)

(define-public (add-verifier (verifier principal) 
                            (name (string-ascii 64))
                            (certification (string-ascii 128)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set authorized-verifiers verifier
            {
                name: name,
                certification: certification,
                active: true,
                verification-count: u0
            }
        )
        (var-set total-verifiers (+ (var-get total-verifiers) u1))
        (ok true)
    )
)

(define-public (deactivate-verifier (verifier principal))
    (let ((verifier-data (unwrap! (map-get? authorized-verifiers verifier) err-verifier-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set authorized-verifiers verifier
            (merge verifier-data { active: false })
        )
        (ok true)
    )
)

(define-public (verify-credit (token-id uint)
                             (trust-score uint)
                             (verification-notes (string-utf8 512))
                             (methodology-used (string-ascii 64)))
    (let ((verifier-data (unwrap! (map-get? authorized-verifiers tx-sender) err-not-authorized)))
        (asserts! (get active verifier-data) err-not-authorized)
        (asserts! (and (>= trust-score u1) (<= trust-score u100)) err-invalid-score)
        (asserts! (is-none (map-get? credit-verifications token-id)) err-already-verified)
        
        (map-set credit-verifications token-id
            {
                verifier: tx-sender,
                verification-date: stacks-block-height,
                trust-score: trust-score,
                verification-notes: verification-notes,
                methodology-used: methodology-used,
                verified: true
            }
        )
        
        (map-set authorized-verifiers tx-sender
            (merge verifier-data 
                { verification-count: (+ (get verification-count verifier-data) u1) }
            )
        )
        
        (var-set total-verifications (+ (var-get total-verifications) u1))
        (ok true)
    )
)

(define-public (update-verification (token-id uint)
                                   (trust-score uint)
                                   (verification-notes (string-utf8 512)))
    (let ((verification-data (unwrap! (map-get? credit-verifications token-id) err-token-not-found))
          (verifier-data (unwrap! (map-get? authorized-verifiers tx-sender) err-not-authorized)))
        (asserts! (get active verifier-data) err-not-authorized)
        (asserts! (is-eq tx-sender (get verifier verification-data)) err-not-authorized)
        (asserts! (and (>= trust-score u1) (<= trust-score u100)) err-invalid-score)
        
        (map-set credit-verifications token-id
            (merge verification-data
                {
                    trust-score: trust-score,
                    verification-notes: verification-notes,
                    verification-date: stacks-block-height
                }
            )
        )
        (ok true)
    )
)

(define-read-only (get-verification-status (token-id uint))
    (map-get? credit-verifications token-id)
)

(define-read-only (is-credit-verified (token-id uint))
    (match (map-get? credit-verifications token-id)
        verification-data (get verified verification-data)
        false
    )
)

(define-read-only (get-credit-trust-score (token-id uint))
    (match (map-get? credit-verifications token-id)
        verification-data (some (get trust-score verification-data))
        none
    )
)

(define-read-only (get-verifier-info (verifier principal))
    (map-get? authorized-verifiers verifier)
)

(define-read-only (is-authorized-verifier (verifier principal))
    (match (map-get? authorized-verifiers verifier)
        verifier-data (get active verifier-data)
        false
    )
)

(define-read-only (get-verifier-stats (verifier principal))
    (match (map-get? authorized-verifiers verifier)
        verifier-data (some (get verification-count verifier-data))
        none
    )
)

(define-read-only (get-verification-summary)
    {
        total-verifiers: (var-get total-verifiers),
        total-verifications: (var-get total-verifications)
    }
)

(define-read-only (get-credits-by-trust-score-range (min-score uint) (max-score uint))
    (ok { min-score: min-score, max-score: max-score })
)