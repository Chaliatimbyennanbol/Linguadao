;; Language Progress Analytics Contract
;; Tracks detailed contributor progress and awards achievement badges

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-EXISTS (err u402))
(define-constant ERR-INVALID-INPUT (err u407))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Achievement badge types
(define-constant BADGE-FIRST-CONTRIBUTION "first-contribution")
(define-constant BADGE-LANGUAGE-PIONEER "language-pioneer")
(define-constant BADGE-DEDICATED-SCHOLAR "dedicated-scholar")
(define-constant BADGE-TRANSLATION-MASTER "translation-master")
(define-constant BADGE-COMMUNITY-LEADER "community-leader")
(define-constant BADGE-MILESTONE-ACHIEVER "milestone-achiever")

;; Data variables
(define-data-var total-analytics-records uint u0)

;; Track detailed progress for each contributor per language
(define-map contributor-language-analytics
  { contributor: principal, language-id: uint }
  {
    total-contributions: uint,
    translation-count: uint,
    recording-count: uint,
    documentation-count: uint,
    teaching-sessions: uint,
    bounties-solved: uint,
    milestones-completed: uint,
    first-contribution-date: uint,
    last-contribution-date: uint,
    total-hours-contributed: uint,
    consistency-score: uint,
    impact-score: uint
  }
)

;; Achievement badges earned by contributors
(define-map contributor-badges
  { contributor: principal }
  {
    badges-earned: (list 20 (string-ascii 30)),
    total-badge-count: uint,
    first-badge-date: uint,
    latest-badge-date: uint,
    rare-badge-count: uint
  }
)

;; Language preservation progress statistics
(define-map language-progress-stats
  { language-id: uint }
  {
    total-contributors: uint,
    total-contributions: uint,
    documentation-percentage: uint,
    translation-percentage: uint,
    audio-recordings: uint,
    active-contributors: uint,
    preservation-score: uint,
    last-activity-date: uint,
    urgency-level: (string-ascii 20)
  }
)

;; Monthly progress tracking for trending analysis
(define-map monthly-progress
  { year: uint, month: uint, language-id: uint }
  {
    contributions-count: uint,
    new-contributors: uint,
    hours-contributed: uint,
    milestones-achieved: uint,
    bounties-completed: uint
  }
)

;; Global leaderboard data
(define-map contributor-rankings
  { contributor: principal }
  {
    global-rank: uint,
    total-languages-contributed: uint,
    total-impact-points: uint,
    specialization-languages: (list 5 uint),
    contribution-streak: uint,
    last-active-date: uint
  }
)

;; Record a new contribution with detailed analytics
(define-public (record-contribution-analytics (contributor principal) (language-id uint) (contribution-type (string-ascii 30)) (hours-spent uint))
  (let
    (
      (existing-analytics (default-to
        {
          total-contributions: u0,
          translation-count: u0,
          recording-count: u0,
          documentation-count: u0,
          teaching-sessions: u0,
          bounties-solved: u0,
          milestones-completed: u0,
          first-contribution-date: stacks-block-height,
          last-contribution-date: stacks-block-height,
          total-hours-contributed: u0,
          consistency-score: u100,
          impact-score: u0
        }
        (map-get? contributor-language-analytics { contributor: contributor, language-id: language-id })
      ))
      (updated-analytics (merge existing-analytics {
        total-contributions: (+ (get total-contributions existing-analytics) u1),
        translation-count: (if (is-eq contribution-type "translation") 
          (+ (get translation-count existing-analytics) u1)
          (get translation-count existing-analytics)),
        recording-count: (if (is-eq contribution-type "recording")
          (+ (get recording-count existing-analytics) u1)
          (get recording-count existing-analytics)),
        documentation-count: (if (is-eq contribution-type "documentation")
          (+ (get documentation-count existing-analytics) u1)
          (get documentation-count existing-analytics)),
        last-contribution-date: stacks-block-height,
        total-hours-contributed: (+ (get total-hours-contributed existing-analytics) hours-spent),
        impact-score: (+ (get impact-score existing-analytics) (* hours-spent u10))
      }))
    )
    (map-set contributor-language-analytics
      { contributor: contributor, language-id: language-id }
      updated-analytics
    )
    
    ;; Update analytics and award badges
    (begin
      (unwrap-panic (update-language-progress-stats language-id))
      (try! (check-and-award-badges contributor language-id updated-analytics))
      (var-set total-analytics-records (+ (var-get total-analytics-records) u1))
      (ok true)
    )
  )
)

;; Award achievement badges based on milestones
(define-public (check-and-award-badges (contributor principal) (language-id uint) (analytics-data (tuple (total-contributions uint) (translation-count uint) (recording-count uint) (documentation-count uint) (teaching-sessions uint) (bounties-solved uint) (milestones-completed uint) (first-contribution-date uint) (last-contribution-date uint) (total-hours-contributed uint) (consistency-score uint) (impact-score uint))))
  (let
    (
      (existing-badges (default-to
        { badges-earned: (list), total-badge-count: u0, first-badge-date: u0, latest-badge-date: u0, rare-badge-count: u0 }
        (map-get? contributor-badges { contributor: contributor })
      ))
      (badges-list (get badges-earned existing-badges))
    )
    
    ;; Award "first-contribution" badge
    (if (and (is-eq (get total-contributions analytics-data) u1) 
             (is-none (index-of badges-list BADGE-FIRST-CONTRIBUTION)))
      (try! (award-badge contributor BADGE-FIRST-CONTRIBUTION))
      true
    )
    
    ;; Award "translation-master" badge for 50+ translations
    (if (and (>= (get translation-count analytics-data) u50)
             (is-none (index-of badges-list BADGE-TRANSLATION-MASTER)))
      (try! (award-badge contributor BADGE-TRANSLATION-MASTER))
      true  
    )
    
    ;; Award "dedicated-scholar" badge for 100+ hours
    (if (and (>= (get total-hours-contributed analytics-data) u100)
             (is-none (index-of badges-list BADGE-DEDICATED-SCHOLAR)))
      (try! (award-badge contributor BADGE-DEDICATED-SCHOLAR))
      true
    )
    
    (ok true)
  )
)

;; Internal function to award a badge
(define-private (award-badge (contributor principal) (badge-type (string-ascii 30)))
  (let
    (
      (existing-badges (default-to
        { badges-earned: (list), total-badge-count: u0, first-badge-date: stacks-block-height, latest-badge-date: u0, rare-badge-count: u0 }
        (map-get? contributor-badges { contributor: contributor })
      ))
      (current-badges (get badges-earned existing-badges))
      (new-badges-list (unwrap! (as-max-len? (append current-badges badge-type) u20) ERR-INVALID-INPUT))
    )
    (map-set contributor-badges
      { contributor: contributor }
      (merge existing-badges {
        badges-earned: new-badges-list,
        total-badge-count: (+ (get total-badge-count existing-badges) u1),
        latest-badge-date: stacks-block-height
      })
    )
    (ok true)
  )
)

;; Update language preservation progress statistics
(define-private (update-language-progress-stats (language-id uint))
  (let
    (
      (existing-stats (default-to
        {
          total-contributors: u0,
          total-contributions: u0,
          documentation-percentage: u0,
          translation-percentage: u0,
          audio-recordings: u0,
          active-contributors: u0,
          preservation-score: u0,
          last-activity-date: stacks-block-height,
          urgency-level: "medium"
        }
        (map-get? language-progress-stats { language-id: language-id })
      ))
    )
    (map-set language-progress-stats
      { language-id: language-id }
      (merge existing-stats {
        total-contributions: (+ (get total-contributions existing-stats) u1),
        last-activity-date: stacks-block-height,
        preservation-score: (+ (get preservation-score existing-stats) u5)
      })
    )
    (ok true)
  )
)

;; Get contributor's analytics for specific language
(define-read-only (get-contributor-analytics (contributor principal) (language-id uint))
  (map-get? contributor-language-analytics { contributor: contributor, language-id: language-id })
)

;; Get contributor's earned badges
(define-read-only (get-contributor-badges (contributor principal))
  (map-get? contributor-badges { contributor: contributor })
)

;; Get language progress statistics
(define-read-only (get-language-stats (language-id uint))
  (map-get? language-progress-stats { language-id: language-id })
)

;; Get monthly progress for trending analysis
(define-read-only (get-monthly-progress (year uint) (month uint) (language-id uint))
  (map-get? monthly-progress { year: year, month: month, language-id: language-id })
)

;; Calculate contributor's global impact score
(define-read-only (calculate-global-impact (contributor principal))
  (ok u0) ;; Simplified for space - would aggregate across all languages
)

;; Get total analytics records count
(define-read-only (get-total-analytics-records)
  (var-get total-analytics-records)
)
