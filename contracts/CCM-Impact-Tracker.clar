;; Carbon Impact Portfolio & Goal Tracking System
;; Enables users to set offset goals, track environmental impact, and monitor progress

(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u400))
(define-constant err-goal-not-found (err u401))
(define-constant err-invalid-target (err u402))
(define-constant err-goal-expired (err u403))
(define-constant err-invalid-period (err u404))
(define-constant err-portfolio-not-found (err u405))
(define-constant err-invalid-credit-amount (err u406))

;; Annual goal periods (blocks per year approximation)
(define-constant blocks-per-year u52560)
(define-constant minimum-goal-target u100)

;; User offset goals tracking
(define-map offset-goals
    { user: principal, goal-year: uint }
    {
        target-credits: uint,
        start-block: uint,
        end-block: uint,
        description: (string-utf8 256),
        goal-type: (string-ascii 32),
        status: (string-ascii 16)
    }
)

;; User portfolio impact tracking
(define-map impact-portfolios
    principal
    {
        total-credits-owned: uint,
        total-credits-retired: uint,
        total-environmental-impact: uint,
        portfolio-value: uint,
        last-updated: uint,
        sustainability-score: uint
    }
)

;; Credit activity logging for impact calculations
(define-map credit-activities
    { user: principal, activity-id: uint }
    {
        token-id: uint,
        activity-type: (string-ascii 16),
        credit-amount: uint,
        impact-score: uint,
        timestamp: uint,
        goal-year: uint
    }
)

;; Achievement tracking for milestones
(define-map user-achievements
    { user: principal, achievement-type: (string-ascii 32) }
    {
        earned-block: uint,
        achievement-value: uint,
        verified: bool
    }
)

;; Global tracking variables
(define-data-var total-active-goals uint u0)
(define-data-var total-portfolios uint u0)
(define-data-var next-activity-id uint u1)
(define-data-var global-impact-score uint u0)

;; Create a new offset goal for a user
(define-public (create-offset-goal (target-credits uint) (goal-year uint) (description (string-utf8 256)) (goal-type (string-ascii 32)))
    (let ((goal-key { user: tx-sender, goal-year: goal-year })
          (current-year (/ stacks-block-height blocks-per-year))
          (start-block (* goal-year blocks-per-year))
          (end-block (+ start-block blocks-per-year)))
        
        ;; Validate goal parameters
        (asserts! (>= target-credits minimum-goal-target) err-invalid-target)
        (asserts! (>= goal-year current-year) err-invalid-period)
        
        ;; Create the goal
        (map-set offset-goals goal-key
            {
                target-credits: target-credits,
                start-block: start-block,
                end-block: end-block,
                description: description,
                goal-type: goal-type,
                status: "active"
            }
        )
        
        ;; Initialize portfolio if it doesn't exist
        (if (is-none (map-get? impact-portfolios tx-sender))
            (begin
                (map-set impact-portfolios tx-sender
                    {
                        total-credits-owned: u0,
                        total-credits-retired: u0,
                        total-environmental-impact: u0,
                        portfolio-value: u0,
                        last-updated: stacks-block-height,
                        sustainability-score: u0
                    }
                )
                (var-set total-portfolios (+ (var-get total-portfolios) u1))
            )
            true
        )
        
        (var-set total-active-goals (+ (var-get total-active-goals) u1))
        (ok true)
    )
)

;; Update goal status (complete, extend, cancel)
(define-public (update-goal-status (goal-year uint) (new-status (string-ascii 16)))
    (let ((goal-key { user: tx-sender, goal-year: goal-year })
          (goal-data (unwrap! (map-get? offset-goals goal-key) err-goal-not-found)))
        
        (map-set offset-goals goal-key
            (merge goal-data { status: new-status })
        )
        
        ;; Track completion achievement
        (if (is-eq new-status "completed")
            (map-set user-achievements
                { user: tx-sender, achievement-type: "goal-completion" }
                {
                    earned-block: stacks-block-height,
                    achievement-value: (get target-credits goal-data),
                    verified: true
                }
            )
            true
        )
        
        (ok true)
    )
)

;; Log credit purchase activity for impact tracking
(define-public (log-credit-purchase (token-id uint) (credit-amount uint) (impact-score uint))
    (let ((activity-id (var-get next-activity-id))
          (current-year (/ stacks-block-height blocks-per-year))
          (portfolio-data (unwrap! (map-get? impact-portfolios tx-sender) err-portfolio-not-found)))
        
        (asserts! (> credit-amount u0) err-invalid-credit-amount)
        
        ;; Log the activity
        (map-set credit-activities
            { user: tx-sender, activity-id: activity-id }
            {
                token-id: token-id,
                activity-type: "purchase",
                credit-amount: credit-amount,
                impact-score: impact-score,
                timestamp: stacks-block-height,
                goal-year: current-year
            }
        )
        
        ;; Update portfolio
        (map-set impact-portfolios tx-sender
            (merge portfolio-data
                {
                    total-credits-owned: (+ (get total-credits-owned portfolio-data) credit-amount),
                    total-environmental-impact: (+ (get total-environmental-impact portfolio-data) impact-score),
                    last-updated: stacks-block-height,
                    sustainability-score: (calculate-sustainability-score 
                        (+ (get total-credits-owned portfolio-data) credit-amount)
                        (get total-credits-retired portfolio-data)
                    )
                }
            )
        )
        
        (var-set next-activity-id (+ activity-id u1))
        (var-set global-impact-score (+ (var-get global-impact-score) impact-score))
        (ok activity-id)
    )
)

;; Log credit retirement activity
(define-public (log-credit-retirement (token-id uint) (credit-amount uint) (impact-score uint))
    (let ((activity-id (var-get next-activity-id))
          (current-year (/ stacks-block-height blocks-per-year))
          (portfolio-data (unwrap! (map-get? impact-portfolios tx-sender) err-portfolio-not-found)))
        
        (asserts! (> credit-amount u0) err-invalid-credit-amount)
        
        ;; Log the retirement activity
        (map-set credit-activities
            { user: tx-sender, activity-id: activity-id }
            {
                token-id: token-id,
                activity-type: "retirement",
                credit-amount: credit-amount,
                impact-score: impact-score,
                timestamp: stacks-block-height,
                goal-year: current-year
            }
        )
        
        ;; Update portfolio
        (map-set impact-portfolios tx-sender
            (merge portfolio-data
                {
                    total-credits-retired: (+ (get total-credits-retired portfolio-data) credit-amount),
                    last-updated: stacks-block-height,
                    sustainability-score: (calculate-sustainability-score 
                        (get total-credits-owned portfolio-data)
                        (+ (get total-credits-retired portfolio-data) credit-amount)
                    )
                }
            )
        )
        
        ;; Check for retirement milestones
        (if (>= (+ (get total-credits-retired portfolio-data) credit-amount) u1000)
            (map-set user-achievements
                { user: tx-sender, achievement-type: "retirement-milestone" }
                {
                    earned-block: stacks-block-height,
                    achievement-value: (+ (get total-credits-retired portfolio-data) credit-amount),
                    verified: true
                }
            )
            true
        )
        
        (var-set next-activity-id (+ activity-id u1))
        (ok activity-id)
    )
)

;; Calculate sustainability score based on ownership and retirement ratios
(define-private (calculate-sustainability-score (total-owned uint) (total-retired uint))
    (if (is-eq total-owned u0)
        u0
        (let ((retirement-ratio (/ (* total-retired u100) total-owned)))
            (if (>= retirement-ratio u50)
                (+ u50 (/ retirement-ratio u2))
                retirement-ratio
            )
        )
    )
)

;; Get user's offset goal for a specific year
(define-read-only (get-offset-goal (user principal) (goal-year uint))
    (map-get? offset-goals { user: user, goal-year: goal-year })
)

;; Get user's impact portfolio summary
(define-read-only (get-impact-portfolio (user principal))
    (map-get? impact-portfolios user)
)

;; Get goal progress calculation
(define-read-only (get-goal-progress (user principal) (goal-year uint))
    (let ((goal-data (map-get? offset-goals { user: user, goal-year: goal-year }))
          (portfolio-data (map-get? impact-portfolios user)))
        (match goal-data
            goal-info
            (match portfolio-data
                portfolio-info
                (let ((progress-percentage (if (> (get target-credits goal-info) u0)
                        (/ (* (get total-credits-retired portfolio-info) u100) (get target-credits goal-info))
                        u0)))
                    (some {
                        target: (get target-credits goal-info),
                        current: (get total-credits-retired portfolio-info),
                        progress-percentage: progress-percentage,
                        status: (get status goal-info),
                        time-remaining: (if (> (get end-block goal-info) stacks-block-height)
                            (- (get end-block goal-info) stacks-block-height)
                            u0)
                    })
                )
                none
            )
            none
        )
    )
)

;; Get user's recent activity
(define-read-only (get-recent-activity (user principal) (activity-id uint))
    (map-get? credit-activities { user: user, activity-id: activity-id })
)

;; Get user achievement
(define-read-only (get-user-achievement (user principal) (achievement-type (string-ascii 32)))
    (map-get? user-achievements { user: user, achievement-type: achievement-type })
)

;; Calculate recommended monthly offset target
(define-read-only (calculate-monthly-target (annual-target uint))
    (/ annual-target u12)
)

;; Get portfolio performance metrics
(define-read-only (get-portfolio-metrics (user principal))
    (match (map-get? impact-portfolios user)
        portfolio-data
        (some {
            sustainability-score: (get sustainability-score portfolio-data),
            impact-efficiency: (if (> (get total-credits-owned portfolio-data) u0)
                (/ (get total-environmental-impact portfolio-data) (get total-credits-owned portfolio-data))
                u0),
            retirement-rate: (if (> (get total-credits-owned portfolio-data) u0)
                (/ (* (get total-credits-retired portfolio-data) u100) (get total-credits-owned portfolio-data))
                u0),
            last-activity: (get last-updated portfolio-data)
        })
        none
    )
)

;; Get global platform statistics
(define-read-only (get-platform-stats)
    {
        total-active-goals: (var-get total-active-goals),
        total-portfolios: (var-get total-portfolios),
        global-impact-score: (var-get global-impact-score),
        next-activity-id: (var-get next-activity-id)
    }
)


