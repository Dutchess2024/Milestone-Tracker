;; File: milestone-tracker.clar
;; A project milestone tracking system for monitoring progress and achievements.
(define-map milestones
  { member: principal }
  { progress: uint, goal: uint, status: uint })

;; Event definitions
(define-constant PROGRESS_EVENT "progress-update")
(define-constant GOAL_SET_EVENT "goal-defined")
(define-constant GOAL_ACHIEVED_EVENT "goal-achieved")

(define-map event-logs
  { type: (string-ascii 20), member: principal }
  { value: uint })

;; Private helper functions to emit custom events. The print function simulates event emission.
(define-private (emit-progress (member principal) (value uint))
    (begin
        (map-insert event-logs { type: PROGRESS_EVENT, member: member } { value: value })
        (print { event: PROGRESS_EVENT, member: member, value: value })
        (ok true)))

;; Allow users to record progress toward their milestone.
(define-public (update-progress (value uint))
    (let ((current-milestone (map-get? milestones { member: tx-sender })))
        (if (is-some current-milestone)
            ;; Update existing progress record
            (let ((milestone-data (unwrap! current-milestone (err "Missing record")))
                  (current-progress (get progress milestone-data))
                  (current-goal (get goal milestone-data))
                  (current-status (get status milestone-data))
                  (new-progress (+ current-progress value)))
                (begin
                    (map-set milestones { member: tx-sender }
                        { progress: new-progress, goal: current-goal, status: current-status })
                    (asserts! (is-ok (emit-progress tx-sender value)) (err "Failed to emit progress event"))
                    (ok value)))
            (begin
                (print { type: "milestone-created", member: tx-sender })
                (map-set milestones { member: tx-sender }
                    { progress: value, goal: u0, status: u0 })
                (asserts! (is-ok (emit-progress tx-sender value)) (err "Failed to emit progress event"))
                (ok value)))))

;; Set a goal for member milestone.
(define-public (define-goal (goal uint))
    (let ((current-milestone (map-get? milestones { member: tx-sender })))
        (if (is-some current-milestone)
            (let ((milestone-data (unwrap! current-milestone (err "Milestone data not found")))
                  (current-progress (get progress milestone-data))
                  (current-status (get status milestone-data)))
                (begin
                    (asserts! (> goal u0) (err "Goal must be greater than zero"))
                    (map-set milestones { member: tx-sender }
                        { progress: current-progress, goal: goal, status: current-status })
                    (print { event: GOAL_SET_EVENT, member: tx-sender, goal: goal })
                    (ok goal)))
            (err "Milestone record not found"))))

;; Check if the member has reached their milestone goal.
(define-public (check-milestone)
    (let ((current-milestone (map-get? milestones { member: tx-sender })))
        (if (is-some current-milestone)
            (let ((milestone-data (unwrap! current-milestone (err "Milestone data not found")))
                  (progress (get progress milestone-data))
                  (goal (get goal milestone-data)))
                (if (>= progress goal)
                    (begin
                        (print { event: GOAL_ACHIEVED_EVENT, member: tx-sender, goal: goal })
                        (ok { status: "Completed", goal: goal, progress: progress }))
                    (ok { status: "In Progress", goal: goal, progress: progress })))
            (err "Milestone record not found"))))

