(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-ALREADY-EXISTS (err u402))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT-FUNDS (err u403))
(define-constant ERR-VOTING-CLOSED (err u405))
(define-constant ERR-ALREADY-VOTED (err u406))
(define-constant ERR-INVALID-AMOUNT (err u407))
(define-constant ERR-PROPOSAL-ACTIVE (err u408))

(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-PROPOSAL-THRESHOLD u1000000)
(define-constant VOTING-PERIOD u1440)
(define-constant REWARD-MULTIPLIER u150)

(define-data-var total-languages uint u0)
(define-data-var total-contributors uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var proposal-counter uint u0)

(define-map languages
  { language-id: uint }
  {
    name: (string-ascii 50),
    description: (string-ascii 200),
    proposer: principal,
    status: (string-ascii 20),
    reward-pool: uint,
    contributors: uint,
    created-at: uint,
    milestones-completed: uint
  }
)

(define-map contributors
  { contributor: principal }
  {
    languages-contributed: (list 20 uint),
    total-rewards: uint,
    reputation-score: uint,
    joined-at: uint,
    active: bool
  }
)

(define-map language-contributions
  { language-id: uint, contributor: principal }
  {
    contribution-type: (string-ascii 30),
    amount-contributed: uint,
    verified: bool,
    timestamp: uint,
    reward-earned: uint
  }
)

(define-map proposals
  { proposal-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 300),
    proposer: principal,
    votes-for: uint,
    votes-against: uint,
    voting-ends: uint,
    executed: bool,
    proposal-type: (string-ascii 20),
    target-language: uint,
    requested-amount: uint
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  { vote: bool, weight: uint }
)

(define-map milestones
  { language-id: uint, milestone-id: uint }
  {
    title: (string-ascii 80),
    description: (string-ascii 150),
    reward-amount: uint,
    completed: bool,
    completed-by: (optional principal),
    completion-date: (optional uint)
  }
)

(define-public (register-language (name (string-ascii 50)) (description (string-ascii 200)))
  (let
    (
      (language-id (+ (var-get total-languages) u1))
      (current-height stacks-block-height)
    )
    (asserts! (>= (stx-get-balance tx-sender) MIN-PROPOSAL-THRESHOLD) ERR-INSUFFICIENT-FUNDS)
    (try! (stx-transfer? MIN-PROPOSAL-THRESHOLD tx-sender (as-contract tx-sender)))
    (map-set languages
      { language-id: language-id }
      {
        name: name,
        description: description,
        proposer: tx-sender,
        status: "proposed",
        reward-pool: MIN-PROPOSAL-THRESHOLD,
        contributors: u0,
        created-at: current-height,
        milestones-completed: u0
      }
    )
    (var-set total-languages language-id)
    (var-set treasury-balance (+ (var-get treasury-balance) MIN-PROPOSAL-THRESHOLD))
    (ok language-id)
  )
)

(define-public (register-contributor)
  (let
    (
      (existing-contributor (map-get? contributors { contributor: tx-sender }))
      (current-height stacks-block-height)
    )
    (asserts! (is-none existing-contributor) ERR-ALREADY-EXISTS)
    (map-set contributors
      { contributor: tx-sender }
      {
        languages-contributed: (list),
        total-rewards: u0,
        reputation-score: u100,
        joined-at: current-height,
        active: true
      }
    )
    (var-set total-contributors (+ (var-get total-contributors) u1))
    (ok true)
  )
)

(define-public (contribute-to-language (language-id uint) (contribution-type (string-ascii 30)) (amount uint))
  (let
    (
      (language (unwrap! (map-get? languages { language-id: language-id }) ERR-NOT-FOUND))
      (contributor-data (unwrap! (map-get? contributors { contributor: tx-sender }) ERR-NOT-FOUND))
      (current-height stacks-block-height)
    )
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (get active contributor-data) ERR-NOT-AUTHORIZED)
    (map-set language-contributions
      { language-id: language-id, contributor: tx-sender }
      {
        contribution-type: contribution-type,
        amount-contributed: amount,
        verified: false,
        timestamp: current-height,
        reward-earned: u0
      }
    )
    (map-set languages
      { language-id: language-id }
      (merge language { contributors: (+ (get contributors language) u1) })
    )
    (ok true)
  )
)

(define-public (verify-contribution (language-id uint) (contributor principal))
  (let
    (
      (contribution (unwrap! (map-get? language-contributions { language-id: language-id, contributor: contributor }) ERR-NOT-FOUND))
      (language (unwrap! (map-get? languages { language-id: language-id }) ERR-NOT-FOUND))
      (contributor-data (unwrap! (map-get? contributors { contributor: contributor }) ERR-NOT-FOUND))
      (reward-amount (* (get amount-contributed contribution) REWARD-MULTIPLIER))
    )
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get proposer language))) ERR-NOT-AUTHORIZED)
    (asserts! (not (get verified contribution)) ERR-ALREADY-EXISTS)
    (asserts! (<= reward-amount (var-get treasury-balance)) ERR-INSUFFICIENT-FUNDS)
    (try! (as-contract (stx-transfer? reward-amount tx-sender contributor)))
    (map-set language-contributions
      { language-id: language-id, contributor: contributor }
      (merge contribution { verified: true, reward-earned: reward-amount })
    )
    (map-set contributors
      { contributor: contributor }
      (merge contributor-data { total-rewards: (+ (get total-rewards contributor-data) reward-amount) })
    )
    (var-set treasury-balance (- (var-get treasury-balance) reward-amount))
    (ok reward-amount)
  )
)

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 300)) (proposal-type (string-ascii 20)) (target-language uint) (requested-amount uint))
  (let
    (
      (proposal-id (+ (var-get proposal-counter) u1))
      (current-height stacks-block-height)
      (contributor-data (unwrap! (map-get? contributors { contributor: tx-sender }) ERR-NOT-FOUND))
    )
    (asserts! (>= (get reputation-score contributor-data) u50) ERR-NOT-AUTHORIZED)
    (map-set proposals
      { proposal-id: proposal-id }
      {
        title: title,
        description: description,
        proposer: tx-sender,
        votes-for: u0,
        votes-against: u0,
        voting-ends: (+ current-height VOTING-PERIOD),
        executed: false,
        proposal-type: proposal-type,
        target-language: target-language,
        requested-amount: requested-amount
      }
    )
    (var-set proposal-counter proposal-id)
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NOT-FOUND))
      (existing-vote (map-get? votes { proposal-id: proposal-id, voter: tx-sender }))
      (contributor-data (unwrap! (map-get? contributors { contributor: tx-sender }) ERR-NOT-FOUND))
      (current-height stacks-block-height)
      (vote-weight (get reputation-score contributor-data))
    )
    (asserts! (< current-height (get voting-ends proposal)) ERR-VOTING-CLOSED)
    (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
    (asserts! (get active contributor-data) ERR-NOT-AUTHORIZED)
    (map-set votes
      { proposal-id: proposal-id, voter: tx-sender }
      { vote: vote-for, weight: vote-weight }
    )
    (if vote-for
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { votes-for: (+ (get votes-for proposal) vote-weight) })
      )
      (map-set proposals
        { proposal-id: proposal-id }
        (merge proposal { votes-against: (+ (get votes-against proposal) vote-weight) })
      )
    )
    (ok true)
  )
)

(define-public (execute-proposal (proposal-id uint))
  (let
    (
      (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-NOT-FOUND))
      (current-height stacks-block-height)
    )
    (asserts! (>= current-height (get voting-ends proposal)) ERR-VOTING-CLOSED)
    (asserts! (not (get executed proposal)) ERR-ALREADY-EXISTS)
    (asserts! (> (get votes-for proposal) (get votes-against proposal)) ERR-NOT-AUTHORIZED)
    (asserts! (<= (get requested-amount proposal) (var-get treasury-balance)) ERR-INSUFFICIENT-FUNDS)
    (if (is-eq (get proposal-type proposal) "funding")
      (begin
        (try! (as-contract (stx-transfer? (get requested-amount proposal) tx-sender (get proposer proposal))))
        (var-set treasury-balance (- (var-get treasury-balance) (get requested-amount proposal)))
      )
      true
    )
    (map-set proposals
      { proposal-id: proposal-id }
      (merge proposal { executed: true })
    )
    (ok true)
  )
)

(define-public (add-milestone (language-id uint) (title (string-ascii 80)) (description (string-ascii 150)) (reward-amount uint))
  (let
    (
      (language (unwrap! (map-get? languages { language-id: language-id }) ERR-NOT-FOUND))
      (milestone-id (+ (get milestones-completed language) u1))
    )
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get proposer language))) ERR-NOT-AUTHORIZED)
    (map-set milestones
      { language-id: language-id, milestone-id: milestone-id }
      {
        title: title,
        description: description,
        reward-amount: reward-amount,
        completed: false,
        completed-by: none,
        completion-date: none
      }
    )
    (ok milestone-id)
  )
)

(define-public (complete-milestone (language-id uint) (milestone-id uint))
  (let
    (
      (milestone (unwrap! (map-get? milestones { language-id: language-id, milestone-id: milestone-id }) ERR-NOT-FOUND))
      (contributor-data (unwrap! (map-get? contributors { contributor: tx-sender }) ERR-NOT-FOUND))
      (current-height stacks-block-height)
      (reward-amount (get reward-amount milestone))
    )
    (asserts! (not (get completed milestone)) ERR-ALREADY-EXISTS)
    (asserts! (get active contributor-data) ERR-NOT-AUTHORIZED)
    (asserts! (<= reward-amount (var-get treasury-balance)) ERR-INSUFFICIENT-FUNDS)
    (try! (as-contract (stx-transfer? reward-amount tx-sender tx-sender)))
    (map-set milestones
      { language-id: language-id, milestone-id: milestone-id }
      (merge milestone { 
        completed: true, 
        completed-by: (some tx-sender),
        completion-date: (some current-height)
      })
    )
    (map-set contributors
      { contributor: tx-sender }
      (merge contributor-data { 
        total-rewards: (+ (get total-rewards contributor-data) reward-amount),
        reputation-score: (+ (get reputation-score contributor-data) u10)
      })
    )
    (var-set treasury-balance (- (var-get treasury-balance) reward-amount))
    (ok reward-amount)
  )
)

(define-public (fund-treasury (amount uint))
  (begin
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    (ok true)
  )
)

(define-read-only (get-language (language-id uint))
  (map-get? languages { language-id: language-id })
)

(define-read-only (get-contributor (contributor principal))
  (map-get? contributors { contributor: contributor })
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-contribution (language-id uint) (contributor principal))
  (map-get? language-contributions { language-id: language-id, contributor: contributor })
)

(define-read-only (get-milestone (language-id uint) (milestone-id uint))
  (map-get? milestones { language-id: language-id, milestone-id: milestone-id })
)

(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

(define-read-only (get-total-languages)
  (var-get total-languages)
)

(define-read-only (get-total-contributors)
  (var-get total-contributors)
)
