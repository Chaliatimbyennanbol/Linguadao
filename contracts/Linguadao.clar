(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-ALREADY-EXISTS (err u402))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT-FUNDS (err u403))
(define-constant ERR-VOTING-CLOSED (err u405))
(define-constant ERR-ALREADY-VOTED (err u406))
(define-constant ERR-INVALID-AMOUNT (err u407))
(define-constant ERR-PROPOSAL-ACTIVE (err u408))
(define-constant ERR-BOUNTY-EXPIRED (err u409))
(define-constant ERR-BOUNTY-CLAIMED (err u410))
(define-constant ERR-INVALID-DIFFICULTY (err u411))
(define-constant ERR-SOLUTION-TOO-SHORT (err u412))

(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-PROPOSAL-THRESHOLD u1000000)
(define-constant VOTING-PERIOD u1440)
(define-constant REWARD-MULTIPLIER u150)

(define-data-var total-languages uint u0)
(define-data-var total-contributors uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var proposal-counter uint u0)
(define-data-var bounty-counter uint u0)

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

(define-map bounties
  { bounty-id: uint }
  {
    title: (string-ascii 100),
    description: (string-ascii 400),
    challenge-text: (string-ascii 500),
    creator: principal,
    language-id: uint,
    difficulty: uint,
    reward-amount: uint,
    expires-at: uint,
    claimed: bool,
    claimed-by: (optional principal),
    claimed-at: (optional uint),
    created-at: uint,
    total-attempts: uint,
    required-reputation: uint
  }
)

(define-map bounty-attempts
  { bounty-id: uint, attempt-id: uint }
  {
    submitter: principal,
    solution-text: (string-ascii 1000),
    submitted-at: uint,
    verified: bool,
    successful: bool
  }
)

(define-map bounty-submissions
  { bounty-id: uint, submitter: principal }
  {
    total-attempts: uint,
    best-attempt: uint,
    last-attempt-at: uint
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

(define-public (create-bounty (title (string-ascii 100)) (description (string-ascii 400)) (challenge-text (string-ascii 500)) (language-id uint) (difficulty uint) (reward-amount uint) (duration uint) (required-reputation uint))
  (let
    (
      (bounty-id (+ (var-get bounty-counter) u1))
      (current-height stacks-block-height)
      (contributor-data (unwrap! (map-get? contributors { contributor: tx-sender }) ERR-NOT-FOUND))
      (language-exists (unwrap! (map-get? languages { language-id: language-id }) ERR-NOT-FOUND))
    )
    (asserts! (and (>= difficulty u1) (<= difficulty u5)) ERR-INVALID-DIFFICULTY)
    (asserts! (> reward-amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= (get reputation-score contributor-data) u75) ERR-NOT-AUTHORIZED)
    (asserts! (>= (stx-get-balance tx-sender) reward-amount) ERR-INSUFFICIENT-FUNDS)
    (try! (stx-transfer? reward-amount tx-sender (as-contract tx-sender)))
    (map-set bounties
      { bounty-id: bounty-id }
      {
        title: title,
        description: description,
        challenge-text: challenge-text,
        creator: tx-sender,
        language-id: language-id,
        difficulty: difficulty,
        reward-amount: reward-amount,
        expires-at: (+ current-height duration),
        claimed: false,
        claimed-by: none,
        claimed-at: none,
        created-at: current-height,
        total-attempts: u0,
        required-reputation: required-reputation
      }
    )
    (var-set bounty-counter bounty-id)
    (var-set treasury-balance (+ (var-get treasury-balance) reward-amount))
    (ok bounty-id)
  )
)

(define-public (submit-bounty-solution (bounty-id uint) (solution-text (string-ascii 1000)))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) ERR-NOT-FOUND))
      (contributor-data (unwrap! (map-get? contributors { contributor: tx-sender }) ERR-NOT-FOUND))
      (current-height stacks-block-height)
      (existing-submission (default-to 
        { total-attempts: u0, best-attempt: u0, last-attempt-at: u0 }
        (map-get? bounty-submissions { bounty-id: bounty-id, submitter: tx-sender })
      ))
      (attempt-id (+ (get total-attempts existing-submission) u1))
    )
    (asserts! (< current-height (get expires-at bounty)) ERR-BOUNTY-EXPIRED)
    (asserts! (not (get claimed bounty)) ERR-BOUNTY-CLAIMED)
    (asserts! (>= (get reputation-score contributor-data) (get required-reputation bounty)) ERR-NOT-AUTHORIZED)
    (asserts! (>= (len solution-text) u20) ERR-SOLUTION-TOO-SHORT)
    (map-set bounty-attempts
      { bounty-id: bounty-id, attempt-id: attempt-id }
      {
        submitter: tx-sender,
        solution-text: solution-text,
        submitted-at: current-height,
        verified: false,
        successful: false
      }
    )
    (map-set bounty-submissions
      { bounty-id: bounty-id, submitter: tx-sender }
      {
        total-attempts: attempt-id,
        best-attempt: attempt-id,
        last-attempt-at: current-height
      }
    )
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { total-attempts: (+ (get total-attempts bounty) u1) })
    )
    (ok attempt-id)
  )
)

(define-public (verify-bounty-solution (bounty-id uint) (attempt-id uint) (successful bool))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) ERR-NOT-FOUND))
      (attempt (unwrap! (map-get? bounty-attempts { bounty-id: bounty-id, attempt-id: attempt-id }) ERR-NOT-FOUND))
      (submitter (get submitter attempt))
      (contributor-data (unwrap! (map-get? contributors { contributor: submitter }) ERR-NOT-FOUND))
      (current-height stacks-block-height)
      (reward-amount (get reward-amount bounty))
      (reputation-bonus (* (get difficulty bounty) u5))
    )
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get creator bounty))) ERR-NOT-AUTHORIZED)
    (asserts! (not (get verified attempt)) ERR-ALREADY-EXISTS)
    (asserts! (not (get claimed bounty)) ERR-BOUNTY-CLAIMED)
    (map-set bounty-attempts
      { bounty-id: bounty-id, attempt-id: attempt-id }
      (merge attempt { verified: true, successful: successful })
    )
    (if successful
      (begin
        (try! (as-contract (stx-transfer? reward-amount tx-sender submitter)))
        (map-set bounties
          { bounty-id: bounty-id }
          (merge bounty { 
            claimed: true,
            claimed-by: (some submitter),
            claimed-at: (some current-height)
          })
        )
        (map-set contributors
          { contributor: submitter }
          (merge contributor-data { 
            total-rewards: (+ (get total-rewards contributor-data) reward-amount),
            reputation-score: (+ (get reputation-score contributor-data) reputation-bonus)
          })
        )
        (var-set treasury-balance (- (var-get treasury-balance) reward-amount))
      )
      true
    )
    (ok successful)
  )
)

(define-public (extend-bounty-deadline (bounty-id uint) (additional-duration uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) ERR-NOT-FOUND))
      (current-height stacks-block-height)
    )
    (asserts! (is-eq tx-sender (get creator bounty)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get claimed bounty)) ERR-BOUNTY-CLAIMED)
    (asserts! (< current-height (get expires-at bounty)) ERR-BOUNTY-EXPIRED)
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { expires-at: (+ (get expires-at bounty) additional-duration) })
    )
    (ok true)
  )
)

(define-public (cancel-bounty (bounty-id uint))
  (let
    (
      (bounty (unwrap! (map-get? bounties { bounty-id: bounty-id }) ERR-NOT-FOUND))
      (current-height stacks-block-height)
      (refund-amount (get reward-amount bounty))
    )
    (asserts! (is-eq tx-sender (get creator bounty)) ERR-NOT-AUTHORIZED)
    (asserts! (not (get claimed bounty)) ERR-BOUNTY-CLAIMED)
    (asserts! (is-eq (get total-attempts bounty) u0) ERR-PROPOSAL-ACTIVE)
    (try! (as-contract (stx-transfer? refund-amount tx-sender (get creator bounty))))
    (map-set bounties
      { bounty-id: bounty-id }
      (merge bounty { claimed: true, claimed-at: (some current-height) })
    )
    (var-set treasury-balance (- (var-get treasury-balance) refund-amount))
    (ok refund-amount)
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

(define-read-only (get-bounty (bounty-id uint))
  (map-get? bounties { bounty-id: bounty-id })
)

(define-read-only (get-bounty-attempt (bounty-id uint) (attempt-id uint))
  (map-get? bounty-attempts { bounty-id: bounty-id, attempt-id: attempt-id })
)

(define-read-only (get-bounty-submission (bounty-id uint) (submitter principal))
  (map-get? bounty-submissions { bounty-id: bounty-id, submitter: submitter })
)

(define-read-only (get-total-bounties)
  (var-get bounty-counter)
)
