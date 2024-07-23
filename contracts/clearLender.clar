
;; Define constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-FUNDED (err u101))
(define-constant ERR-INSUFFICIENT-FUNDS (err u102))
(define-constant ERR-LOAN-NOT-ACTIVE (err u103))
(define-constant ERR-LOAN-NOT-DUE (err u104))

;; Define data maps
(define-map loans 
  { loan-id: uint }
  {
    borrower: principal,
    lender: (optional principal),
    amount: uint,
    interest-rate: uint,
    term: uint,
    start-block: (optional uint),
    status: (string-ascii 20)
  }
)

(define-map user-balances principal uint)

;; Define functions
(define-public (create-loan (amount uint) (interest-rate uint) (term uint))
  (let ((loan-id (+ (var-get loan-counter) u1)))
    (map-set loans 
      { loan-id: loan-id }
      {
        borrower: tx-sender,
        lender: none,
        amount: amount,
        interest-rate: interest-rate,
        term: term,
        start-block: none,
        status: "OPEN"
      }
    )
    (var-set loan-counter loan-id)
    (ok loan-id)
  )
)

(define-public (fund-loan (loan-id uint))
  (let (
    (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-ACTIVE))
    (lender-balance (default-to u0 (map-get? user-balances tx-sender)))
  )
    (asserts! (is-none (get lender loan)) ERR-ALREADY-FUNDED)
    (asserts! (>= lender-balance (get amount loan)) ERR-INSUFFICIENT-FUNDS)
    
    (map-set loans { loan-id: loan-id }
      (merge loan { 
        lender: (some tx-sender),
        start-block: (some block-height),
        status: "ACTIVE"
      })
    )
    (map-set user-balances tx-sender (- lender-balance (get amount loan)))
    (map-set user-balances (get borrower loan) (+ (default-to u0 (map-get? user-balances (get borrower loan))) (get amount loan)))
    (ok true)
  )
)

(define-public (repay-loan (loan-id uint))
  (let (
    (loan (unwrap! (map-get? loans { loan-id: loan-id }) ERR-LOAN-NOT-ACTIVE))
    (borrower-balance (default-to u0 (map-get? user-balances tx-sender)))
    (repayment-amount (+ (get amount loan) (/ (* (get amount loan) (get interest-rate loan)) u100)))
  )
    (asserts! (is-eq (get borrower loan) tx-sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (get status loan) "ACTIVE") ERR-LOAN-NOT-ACTIVE)
    (asserts! (>= (- block-height (unwrap! (get start-block loan) ERR-LOAN-NOT-ACTIVE)) (get term loan)) ERR-LOAN-NOT-DUE)
    (asserts! (>= borrower-balance repayment-amount) ERR-INSUFFICIENT-FUNDS)
    
    (map-set user-balances tx-sender (- borrower-balance repayment-amount))
    (map-set user-balances (unwrap! (get lender loan) ERR-LOAN-NOT-ACTIVE) (+ (default-to u0 (map-get? user-balances (unwrap! (get lender loan) ERR-LOAN-NOT-ACTIVE))) repayment-amount))
    (map-set loans { loan-id: loan-id } (merge loan { status: "REPAID" }))
    (ok true)
  )
)

;; Read-only functions
(define-read-only (get-loan (loan-id uint))
  (map-get? loans { loan-id: loan-id })
)

(define-read-only (get-balance (user principal))
  (default-to u0 (map-get? user-balances user))
)

;; Initialize loan counter
(define-data-var loan-counter uint u0)