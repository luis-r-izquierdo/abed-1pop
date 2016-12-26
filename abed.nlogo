;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ABED
;; ABED (Agent-Based Evolutionary Dynamics) is a modeling framework designed
;; to simulate the evolution of a population of agents who play a
;; symmetric 2-player game and, from time to time, are given the
;; opportunity to revise their strategy.
;; Copyright (C) 2016 Luis R. Izquierdo, Segismundo S. Izquierdo & Bill Sandholm
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Contact information:
;; Luis R. Izquierdo
;;   University of Burgos, Spain.
;;   e-mail: lrizquierdo@ubu.es

extensions [rnd]
;; for rnd:weighted-one-of, for clarity of code
;; (this can be done equally efficiently using rd-index-by-weights-from)


;;;;;;;;;;;;;;;;;
;;; Variables ;;;
;;;;;;;;;;;;;;;;;

globals [
  payoffs               ;; this is the payoff matrix
  n-of-strategies
  strategies-payoffs

  ;; plotting
  ticks-per-second
  plotting-period
  second-to-plot

  ;; tasks
  follow-rule
  update-payoff
  update-candidates-and-payoffs
  update-candidate-agents
  update-counterparts
  reported-counterparts
  tie-winner-in

  ;; for proportional
  rate-scaling

  max-size-of-test-set ;; for direct protocols
  max-number-to-consider-imitating ;; for imitative protocols

  strategy-numbers ;; for efficiency
  agents-list ;; for efficiency

  list-of-parameters ;; to save and load parameters

  ;; for random-walk tie-breaker
  rw-st-freq-non-committed-agents
]


breed [players player]
breed [strategy-agents strategy-agent]

players-own [
  strategy      ;; an integer >= 1
  next-strategy ;; to model synchronous revision
  payoff

  counterparts ;; list with the agents to play with.
               ;; counterparts is a list rather than a set so we can deal with replacement.
               ;; Lists can contain duplicates, but agentsets cannot.
  other-agents-list
               ;; this list is useful to create the counterparts if not self-matching?
  population-to-play-with
               ;; list with the agents to play with:
               ;; either agents-list (if self-matching?) or other-agents-list (if not self-matching?)

  candidates   ;; list (or agentset) containing the group of entities you are going to select from.
               ;; Entities are agents if candidate-selection = imitative, and
               ;; in that case candidates is a list of agents (so we can deal with replacement,
               ;; since lists can contain duplicates, but agentsets cannot.)
               ;; Entities are strategies if candidate-selection = direct, and
               ;; in that case candidates is an agentset of strategy-agents.
               ;;
  population-to-imitate-to
               ;; list with the agents to imitate to:
               ;; either agents (if consider-imitating-self?) or other-agents-list (if not consider-imitating-self?)

  played?      ;; true if the agent has played in this tick, false otherwise
]

strategy-agents-own [
  strategy     ;; an integer >= 1
  payoff
  counterparts ;; list with the agents to play with.
               ;; counterparts is a list rather than a set so we can deal with replacement.
               ;; Lists can contain duplicates, but agentsets cannot.
]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to startup
  clear-all
  no-display

  setup-payoffs

  carefully [
    setup-agents
    setup-strategy-agents

    setup-dynamics

    update-ticks-per-second
    update-strategies-payoffs

    reset-ticks
    setup-graphs

    setup-list-of-parameters
    setup-random-walk
  ]
  [print error-message]

end

to setup-agents
  ifelse random-initial-condition?
  [
    create-players n-of-agents [
      set payoff 0
      set strategy 1 + random n-of-strategies
      set next-strategy strategy ;; to make sure that if you do not change next-strategy, you keep the same strategy
      set hidden? true
    ]
  ]
  [
    let initial-distribution read-from-string initial-condition

    let i 0
    foreach initial-distribution [
      [x] ->
      create-players x [
        set payoff 0
        set strategy (i + 1)
        set next-strategy strategy ;; to make sure that if you do not change next-strategy, you keep the same strategy
        set hidden? true
      ]
      set i (i + 1)
    ]
     set n-of-agents count players
  ]

  ask players [set other-agents-list sort other players]
  set agents-list sort players
  set n-of-revisions-per-tick min (list n-of-revisions-per-tick n-of-agents)
end

to setup-strategy-agents
  let i 0
  create-strategy-agents n-of-strategies [
    set payoff 0
    set hidden? true
    set strategy (1 + i)
    set i (i + 1)
  ]
end

to setup-dynamics

  ;; SELECT YOUR NEXT STRATEGY DIRECTLY, OR VIA IMITATION
  ifelse (candidate-selection = "direct")
    [ set update-candidates-and-payoffs [ [] -> update-candidate-strategies-and-payoffs ] ]
    [ set update-candidates-and-payoffs [ [] -> update-candidate-agents-and-payoffs ] ]

  ;; NUMBER OF STRATEGIES YOU WILL TEST (ONLY RELEVANT IN DIRECT PROTOCOLS)
  set n-in-test-set min list n-in-test-set n-of-strategies
  set max-size-of-test-set min list 10 n-of-strategies

  ;; NUMBER OF AGENTS YOU WILL CONSIDER FOR IMITATION (ONLY RELEVANT IN IMITATIVE PROTOCOLS)
  set n-to-consider-imitating min list n-to-consider-imitating (n-of-agents - 1)
  set max-number-to-consider-imitating min list 10 (n-of-agents - 1)

  ;; RULE USED TO SELECT AMONG DIFFERENT CANDIDATES
  set follow-rule runresult (word "[ [] -> " decision-method " ]")

  ;; TIE-BREAKER
  set tie-winner-in runresult (word "[ [x] -> " tie-breaker " x ]")

  ;; DO YOU PLAY YOURSELF?
  ifelse self-matching?
    [ ask players [ set population-to-play-with agents-list ] ]
    [ ask players [ set population-to-play-with other-agents-list ] ]

  if not trials-with-replacement? [
    set n-of-trials min list n-of-trials (n-of-agents - ifelse-value self-matching? [0][1])
  ]

  ;; DO YOU PLAY EVERYONE?
  ifelse complete-matching?
    [
      set update-payoff [ [] -> update-payoff-full-matching ]
      set n-of-trials ifelse-value self-matching? [count players] [count players - 1]
      set trials-with-replacement? false
      set single-sample? true
    ]
    [ set update-payoff [ [] -> update-payoff-not-full-matching ] ]

  ;; DO YOU DRAW A DIFFERENT SAMPLE OF AGENTS TO PLAY WITH EVERY TIME YOU TEST A STRATEGY,
  ;; OR JUST ONE SINGLE SAMPLE FOR ALL YOUR TESTS? (ONLY RELEVANT IN DIRECT PROTOCOLS)
  ifelse single-sample?
    [ set reported-counterparts [ [] -> fixed-counterparts ] ]
    [ set reported-counterparts [ [] -> variable-counterparts ] ]

  ;; DO YOU SELECT THE AGENTS YOU ARE GOING TO PLAY WITH REPLACEMENT OR WITHOUT REPLACEMENT?
  ifelse trials-with-replacement?
    [set update-counterparts [ [] -> update-counterparts-with-replacement ] ]
    [set update-counterparts [ [] -> update-counterparts-without-replacement ] ]

  ifelse imitatees-with-replacement?
    [set update-candidate-agents [ [] -> update-candidate-agents-with-replacement ] ]
    [
      set update-candidate-agents [ [] -> update-candidate-agents-without-replacement ]
      set consider-imitating-self? false
        ;; if there is no replacement, you cannot form part of the candidate strategies again
        ;; (note that you always consider yourself)
    ]

  ifelse consider-imitating-self? and imitatees-with-replacement?
    [ask players [ set population-to-imitate-to agents-list] ]
    [ask players [ set population-to-imitate-to other-agents-list] ]

end


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run-time procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go

  ;; the following two lines can be commented out if parameter values are not
  ;; going to change over the course of the simulation
  setup-dynamics
  update-ticks-per-second

  update-strategies-payoffs

  ask players [set played? false]
  ifelse use-prob-revision?
    [ask players with [random-float 1 < prob-revision] [update-strategy]]
    [ask n-of n-of-revisions-per-tick players [update-strategy]]

  tick
  ask players [set strategy next-strategy]

  if (ticks mod (ceiling plotting-period) = 0) [update-graphs]

  update-num-agents

  if (decision-method = "best" and tie-breaker = "random-walk") [
    repeat (floor (n-of-agents * random-walk-speed)) [advance-random-walk]
  ]

end

to update-ticks-per-second
  ;; it is assumed that, on average, all agents revise once per second
  set ticks-per-second ifelse-value use-prob-revision?
    [ 1 / prob-revision]
    [ n-of-agents / n-of-revisions-per-tick]

  if plot-every-?-secs < 1 / ticks-per-second [set plot-every-?-secs 1 / ticks-per-second]
  set plotting-period (ticks-per-second * plot-every-?-secs)
end

to update-num-agents
  let diff (n-of-agents - count players)

  if diff != 0 [
    ifelse diff > 0
    [ repeat diff [ ask one-of players [hatch-players 1] ] ]
    [
      ask n-of (- diff) players [die]
      set n-of-trials min list n-of-trials (n-of-agents - ifelse-value self-matching? [0][1])
    ]
    ask players [set other-agents-list sort other players]
    set agents-list sort players
    set n-of-revisions-per-tick min (list n-of-revisions-per-tick n-of-agents)
    setup-random-walk ;; since the number of agents has changed
  ]
end

;;;;;;;;;;;;;;;
;;; PAYOFFS ;;;
;;;;;;;;;;;;;;;

to setup-payoffs
  set payoffs read-from-string payoff-matrix
  set n-of-strategies length payoffs

  if (not all-equal? map length payoffs) [
    user-message "The payoff matrix should be square"
  ]

  carefully [
    if (not all-equal? map length transpose-of payoffs) [
      user-message "The payoff matrix should be square"
    ]

    if length transpose-of payoffs != n-of-strategies [
      user-message (word "The payoff matrix should be square.\n Currently it has " n-of-strategies
        " rows and " length transpose-of payoffs " columns"
        )
    ]

    set strategy-numbers (range 1 (n-of-strategies + 1))
    set rate-scaling max-column-difference payoffs

    let initial-distribution read-from-string initial-condition
    if length initial-distribution != n-of-strategies [
      user-message (word "The number of items in initial-condition (i.e. " length initial-distribution "):\n"
        initial-condition "\nshould be equal to the number of rows (and columns) of the payoff matrix (i.e. " n-of-strategies "):\n"
        payoffs
        )
    ]

    if length filter [ [x] -> x < 0] initial-distribution > 0 [
      user-message (word "All numbers in " initial-distribution "\nshould be non-negative numbers")
    ]

  ] [print error-message]

end


to update-strategies-payoffs
  let strategy-counts map [ [s] -> count players with [strategy = s]] strategy-numbers
    ;; if nobody is playing a strategy, there's no problem in this model
  let current-agg-payoffs map [ [s] -> sum (map * strategy-counts (item (s - 1) payoffs)) - (ifelse-value self-matching? [0] [item (s - 1) item (s - 1) payoffs]) ] strategy-numbers
  set strategies-payoffs map [ [p] -> p / (n-of-agents - (ifelse-value self-matching? [0][1]))] current-agg-payoffs
    ;; useful relevant notes in Sandholm (2010, "Population Games and Evolutionary Dynamics", section 11.4.1, pp. 418-419)
end

to have-payoff-ready
  if not played? [
    run update-payoff
    set played? true
  ]
end

to update-counterparts-with-replacement
  set counterparts n-values n-of-trials [one-of population-to-play-with]
end

to update-counterparts-without-replacement
  set counterparts n-of n-of-trials population-to-play-with
end

to update-payoff-full-matching
  set payoff item (strategy - 1) strategies-payoffs
end

to update-payoff-not-full-matching
  run update-counterparts
  let my-payoffs item (strategy - 1) payoffs
  let total-payoff sum (map * my-payoffs (strategy-freq counterparts))
  set payoff total-payoff / n-of-trials
end

to-report strategy-freq [list-of-agents]
  let str-freq n-values n-of-strategies [0]
  foreach list-of-agents [ [ag] ->
    let str ([strategy] of ag) - 1
    set str-freq replace-item str str-freq ((item str str-freq) + 1)
  ]
  report str-freq
end

;;;;;;;;;;;;;;;;;;;;;;;
;;; UPDATE-STRATEGY ;;;
;;;;;;;;;;;;;;;;;;;;;;;

to update-strategy
  ifelse random-float 1 < prob-mutation
  [set next-strategy (1 + random n-of-strategies)]
  [run follow-rule]
end

to update-candidate-agents-and-payoffs
  run update-candidate-agents
   ;; note that candidates is a list to select from, and you are always added to it.
   ;; candidates could have duplicates if imitatees-with-replacement? is on.
  have-payoff-ready
  ask (turtle-set candidates) [have-payoff-ready]
end

to update-candidate-agents-with-replacement
  set candidates (fput self (n-values n-to-consider-imitating [one-of population-to-imitate-to]))
end

to update-candidate-agents-without-replacement
  set candidates (fput self (n-of n-to-consider-imitating population-to-imitate-to))
end

to update-candidate-strategies-and-payoffs
  let my-strategy-agent one-of (strategy-agents with [strategy = [strategy] of myself])
  set candidates (turtle-set
    my-strategy-agent
    n-of (n-in-test-set - 1) (strategy-agents with [strategy != [strategy] of myself])
  )
  ;; here candidates is an agentset (which contains strategy-agents)

  update-payoffs-of-strategy-agents candidates
  set payoff [payoff] of my-strategy-agent

end

to update-payoffs-of-strategy-agents [strategy-set]
  ;; We assume clever payoff evaluation, i.e. to compute the payoff of a strategy
  ;; that the revising agent is not using, we imagine that the agent switches to this new strategy,
  ;; and then we compute the payoff of this new strategy in this new state.
  ;; Useful relevant notes in Sandholm (2010, "Population Games and Evolutionary Dynamics", section 11.4.2, pp. 419-421)

  let ag-strategy strategy

  ifelse complete-matching?

  [
    ask strategy-set [
      let st-strategy strategy
      ;; ask the agent to adopt this strategy, so payoffs are computed
      ;; under the hypothesis that the agent has switched (clever payoff evaluation).
      ask myself [set strategy st-strategy]

      update-strategies-payoffs
      ;; we have to compute the strategies-payoffs again since strategy-counts
      ;; is different in the new state
      set payoff item (strategy - 1) strategies-payoffs
    ]
  ]

  [
    run update-counterparts
    ask strategy-set [
      set counterparts runresult reported-counterparts
        ;; reported-counterparts can be fixed-counterparts (if single-sample?)
        ;; or variable-counterparts (if not single-sample?)

      let st-strategy strategy
      ;; ask the agent to adopt this strategy, so payoffs are computed
      ;; under the hypothesis that the agent has switched (clever payoff evaluation).
      ;; This is also relevant if the agent himself is part of the set of counterparts.
      ask myself [set strategy st-strategy]

      let my-payoffs item (strategy - 1) payoffs
      let total-payoff sum (map * my-payoffs (strategy-freq counterparts))
      ;; Note that the revising agent could be part of counterparts (if self-matching? is on).
      ;; This, together with clever payoff evaluation (see above), implies that even when using
      ;; single-sample? each strategy may be evaluated against a different set of sample strategies,
      ;; since the agent is switching strategy before evaluating payoffs.
      set payoff total-payoff / n-of-trials
    ]
  ]

  set strategy ag-strategy
  if complete-matching? [update-strategies-payoffs]
end

;; POSSIBLE VALUES OF reported-counterparts

to-report fixed-counterparts
  report [counterparts] of myself
end

to-report variable-counterparts
  ask myself [run update-counterparts]
  report [counterparts] of myself
end

;; DECISION-METHODS

to best
  run update-candidates-and-payoffs
  let best-candidates items-with-max-payoff-in sort candidates
   ;; candidates here may be a list of agents (if candidate-selection = imitative), or
   ;; an agentset of strategy-agents (if candidate-selection = direct).
   ;; We cannot write ((turtle-set candidates) with-max [payoff]) because agentsets cannot contain duplicates,
   ;; and this is a problem if imitatees-with-replacement? is on.
  set next-strategy (runresult tie-winner-in map [ [c] -> [strategy] of c] best-candidates)
end

to proportional
  ;; useful relevant notes in Sandholm (2010, "Population Games and Evolutionary Dynamics", section 4.3.1, pp. 126-127)

  if rate-scaling != 0 [
    ;; rate-scaling is zero only if the whole payoff matrix is 0s.
    ;; In that case there is nothing to do here.

    ifelse candidate-selection = "direct" [set n-in-test-set 2] [set n-to-consider-imitating 1]
    run update-candidates-and-payoffs

    let sorted-candidates sort-on [payoff] (turtle-set candidates)
    let worse first sorted-candidates
    let better last sorted-candidates
    let payoff-diff ([payoff] of better - [payoff] of worse)

    if random-float 1 < (payoff-diff / rate-scaling) [
      set next-strategy [strategy] of better
    ]
    ;; If your strategy is the better, you are going to stick with it
    ;; If it's not, you switch with probability (payoff-diff / rate-scaling)
  ]
end

to logit
  run update-candidates-and-payoffs
  carefully [
    let candidate-to-imitate rnd:weighted-one-of-list (sort candidates) [ [c] -> exp (([payoff] of c) / eta)]
    ;; candidates here may be a list of agents (if candidate-selection = imitative), or
    ;; an agentset of strategy-agents (if candidate-selection = direct).
    set next-strategy [strategy] of candidate-to-imitate
  ]
  [
    user-message "Logit has computed a number that is too big for IEEE 754 floating-point computation\nPlease consider using a lower value for eta."
    print error-message
  ]
end


;; TIE-BREAKERS

to-report stick-uniform [st-list]
  report ifelse-value member? strategy st-list [strategy] [one-of st-list]
end

to-report stick-min [st-list]
  report ifelse-value member? strategy st-list [strategy] [min st-list]
end

to-report uniform [st-list]
  report one-of st-list
end

to-report random-walk [st-list]
    ;; useful relevant notes in Sandholm (2010, "Population Games and Evolutionary Dynamics", section 11.4.3, pp. 421-423)
  report rnd:weighted-one-of-list (remove-duplicates st-list) [ [s] -> 1 + item (s - 1) rw-st-freq-non-committed-agents]
    ;; We add one to the weights to account for the non-committed agents
end

to setup-random-walk
  set rw-st-freq-non-committed-agents tally-strategies n-values n-of-agents [1 + random n-of-strategies]
    ;; this list is n-of-strategies long and, initially, it is a random distribution
end

to advance-random-walk
  let imitator-st rnd:weighted-one-of-list strategy-numbers [ [s] -> item (s - 1) rw-st-freq-non-committed-agents]
    ;; imitator-st is intended to represent the strategy of
    ;; the agent who has been chosen to revise his strategy.
  let rw-st-freq-imitatees subtract-one-in-pos-?1-of-list-?2 (imitator-st - 1) rw-st-freq-non-committed-agents
    ;; rw-st-freq-imitatees is the strategy distribution
    ;; of the non-committed agents who may be chosen to be imitated
  let new-strategy rnd:weighted-one-of-list strategy-numbers [ [s] -> 1 + item (s - 1) rw-st-freq-imitatees]
    ;; We add one to the weights to account for the non-committed agents
  set rw-st-freq-non-committed-agents add-one-in-pos-?1-of-list-?2 (new-strategy - 1) rw-st-freq-imitatees
end


;;;;;;;;;;;;;;
;;; GRAPHS ;;;
;;;;;;;;;;;;;;

to setup-graphs
  setup-miliseconds-graph "Strategy distributions (recent history)" 1
  setup-graph "Strategy distributions (complete history)" 1

  setup-miliseconds-graph "Strategies' expected payoff (recent history)" 0
  setup-graph "Strategies' expected payoff (complete history)" 0

  update-graphs
end

to setup-graph [s mode]
  set-current-plot s
  foreach strategy-numbers [ [n] ->
    create-temporary-plot-pen (word n)
    set-plot-pen-mode mode
    set-plot-pen-interval plot-every-?-secs
    set-plot-pen-color 25 + 40 * (n - 1)
  ]
end

to setup-miliseconds-graph [s mode]
  set-current-plot s
  foreach strategy-numbers [ [n] ->
    create-temporary-plot-pen (word n)
    set-plot-pen-mode mode
    set-plot-pen-interval 1000 * plot-every-?-secs
    set-plot-pen-color 25 + 40 * (n - 1)
  ]
end

to update-graphs
  let strategy-counts map [ [s] -> count players with [strategy = s]] strategy-numbers
  let strategy-frequencies map [ [n] -> n / n-of-agents] strategy-counts
  let strategies-expected-payoff map [ [s] -> sum (map * strategy-frequencies (item (s - 1) payoffs)) ] strategy-numbers

  if show-recent-history? [
    set-current-plot "Strategy distributions (recent history)"
      plot-frequencies-?-at-? strategy-frequencies (1000 * second-to-plot)
      fix-x-range

    set-current-plot "Strategies' expected payoff (recent history)"
      foreach strategy-numbers [ [s] ->
        set-current-plot-pen (word s)
        ;; set-plot-pen-interval plot-every-?-ticks
        plotxy (1000 * second-to-plot) item (s - 1) strategies-expected-payoff
      ]
      fix-x-range
  ]

  if show-complete-history? [
    set-current-plot "Strategy distributions (complete history)"
      plot-frequencies-?-at-? strategy-frequencies second-to-plot

    set-current-plot "Strategies' expected payoff (complete history)"
      foreach strategy-numbers [ [s] ->
        set-current-plot-pen (word s)
        ;; set-plot-pen-interval plot-every-?-ticks
        plotxy second-to-plot item (s - 1) strategies-expected-payoff
      ]
  ]

  set second-to-plot (second-to-plot + plot-every-?-secs)
end

to fix-x-range
  set-plot-x-range floor (max (list 0 (1000 * (second-to-plot - duration-of-recent)))) floor (1000 * second-to-plot + (1000 * plot-every-?-secs))
end

to plot-frequencies-?-at-? [freq x]
  let bar 1
  foreach strategy-numbers [ [s] ->
    set-current-plot-pen (word s)
    ;; set-plot-pen-interval plot-every-?-secs
    plotxy x bar
    set bar (bar - (item (s - 1) freq))
  ]
  set-plot-y-range 0 1
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SUPPORTING PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;;; Matrices ;;;
;;;;;;;;;;;;;;;;

to-report max-column-difference [m]
  let mt transpose-of m
  report max-row-difference mt
end

to-report max-row-difference [m]
  report max n-values (length m) [ [n] -> max (item n m) - min (item n m)]
end

to-report transpose-of [m]
  let n-rows length m
  let n-cols length first m
  let mt n-values n-cols [n-values n-rows [0]]

  let r 0
  foreach m [ [m-row] ->
    let c 0
    foreach m-row [ [n] ->
      set mt replace-item c mt (replace-item r (item c mt) n)
      set c (c + 1)
    ]
    set r (r + 1)
  ]
  report mt
end

;;;;;;;;;;;;;
;;; Lists ;;;
;;;;;;;;;;;;;

to-report all-equal? [l]
  let first-element first l
  report reduce and (map [ [el] -> el = first-element] l)
end

to-report max-positions [numbers]
  let biggest max numbers
  report filter [ [n] -> item n numbers = biggest] (range (length numbers))
end

to-report items-with-max-payoff-in [l]
  let items []
  let max-payoff [payoff] of first l
  foreach l [ [el] ->
    let payoff-of-item [payoff] of el
    if payoff-of-item >= max-payoff [
      ifelse payoff-of-item = max-payoff
        [ set items lput el items ]
        [
          set max-payoff payoff-of-item
          set items (list el)
        ]
    ]
  ]
  report items
end

to-report tally [l]
  ;; it is assumed that the l is a list that contains natural numbers only
  let M max l
  let h n-values (M + 1) [0]
  foreach l [ [el] ->
    set h replace-item el h ((item el h) + 1)
    ]
  report h
end

to-report tally-strategies [l]
  ;; it is assumed that the l is a list that contains strategy numbers only
  let h n-values n-of-strategies [0]
  foreach l [ [s] ->
    set h replace-item (s - 1) h ((item (s - 1) h) + 1)
    ]
  report h
end

to-report subtract-one-in-pos-?1-of-list-?2 [pos l]
  report replace-item pos l ((item pos l) - 1)
end

to-report add-one-in-pos-?1-of-list-?2 [pos l]
  report replace-item pos l ((item pos l) + 1)
end



;;;;;;;;;;;;;;;;;;
;; Output files ;;
;;;;;;;;;;;;;;;;;;

to setup-list-of-parameters
  set list-of-parameters (list
    "payoff-matrix"
    "n-of-agents"
    "random-initial-condition?"
    "initial-condition"
    "candidate-selection"
    "decision-method"
    "complete-matching?"
    "n-of-trials"
    "single-sample?"
    "n-in-test-set"
    "tie-breaker"
    "eta"
    "random-walk-speed"
    "n-to-consider-imitating"
    "use-prob-revision?"
    "prob-revision"
    "n-of-revisions-per-tick"
    "prob-mutation"
    "trials-with-replacement?"
    "self-matching?"
    "imitatees-with-replacement?"
    "consider-imitating-self?"
    "plot-every-?-secs"
    "duration-of-recent"
    "show-recent-history?"
    "show-complete-history?"
    )
end


;; This procedure loads in data from a text file and sets the variables accordingly.
to load-parameter-file
  let file user-file

  ;; Note that we need to check that file isn't false. user-file
  ;; will return false if the user cancels the file dialog.
  if ( file != false )
  [
    ;; This opens the file, so we can use it.
    file-open file

    ;; Read in the file (assumed to be in exactly the same format as when saved )
    while [not file-at-end?]
    [
      let string file-read-line
      let comma-position position "," string
      let variable-name substring string 0 comma-position
      let value substring string (comma-position + 1) (length string)
      run (word "set " variable-name " " value)
    ]

    set payoff-matrix put-sublists-in-different-lines payoff-matrix

    user-message "File loading complete!"

    ;; Done reading in the information.  Close the file.
    file-close

    startup
  ]

end

to-report put-sublists-in-different-lines [s]
  let open-bracket-pos position "[" s
  set s substring s (open-bracket-pos + 1) (length s)
  let close-bracket-pos -1

  let new-s "["

  set open-bracket-pos position "[" s
  while [open-bracket-pos != false] [
    set close-bracket-pos position "]" s
    set new-s (word new-s (substring s open-bracket-pos (close-bracket-pos + 1)) "\n ")
    set s substring s (close-bracket-pos + 1) (length s)
    set open-bracket-pos position "[" s
  ]
  report (word substring new-s 0 (length new-s - 2) "]")

end

;; This procedure saves the parameters into a new file
;; or appends the data to an existing file
to save-parameter-file
  let file user-new-file

  if ( file != false )
  [
    carefully [file-delete file] [] ;; overwrite the file if it exists
    file-open file

    foreach list-of-parameters [ [p] -> file-print (word p "," (fix-string runresult p)) ]
    file-close
  ]
end

to-report fix-string [s]
  ;;report ifelse-value is-string? s [remove "\n" s][s]
  report ifelse-value is-string? s [ (word "\"" (remove "\n" s)  "\"") ] [s]
end

;;;;;;;;;;;;;;;;;;;;;;
;;; Random numbers ;;;
;;;;;;;;;;;;;;;;;;;;;;

;to-report cum-list-from [w]
;  let cum-list (list first w)
;  ;; cum-list first value is the first value of w, and it is as long as w
;  foreach but-first w [set cum-list lput (? + last cum-list) cum-list]
;  report cum-list
;end
;
;to-report rd-index-by-cumulative-weights [cw]
;  let rd-index 0
;  let tmp random-float last cw
;  ;; select the new strategy with probability proportional to the elements of i-d
;  foreach cw [ if (tmp > ?) [set rd-index (rd-index + 1)] ]
;  report rd-index
;end
;
;to-report rd-index-by-weights-from [w]
;  report rd-index-by-cumulative-weights (cum-list-from w)
;end

;; if speed is critical, consider using extension rnd (https://github.com/NetLogo/Rnd-Extension)
;; The extension uses Keith Schwarz's implementation of Vose's Alias Method (see http://www.keithschwarz.com/darts-dice-coins/).
;; Assuming you are choosing n candidates for a collection of size m with repeats, this method has an initialization cost of O(m),
;; followed by a cost of O(1) for each item you pick, so O(m + n) overall.
;; rnd:weighted-n-of-list-with-repeats implements

;; examples and speed comparisons in file random-sampling-weights.nlogo
@#$#@#$#@
GRAPHICS-WINDOW
524
78
725
280
-1
-1
64.33333333333334
1
10
1
1
1
0
0
0
1
-1
1
-1
1
0
0
1
ticks
30.0

INPUTBOX
25
341
230
488
payoff-matrix
[[ 6 0 0 ]\n [ 5 7 5 ]\n [ 0 5 8 ]]
1
1
String (reporter)

SLIDER
26
502
230
535
n-of-agents
n-of-agents
1
5000
300.0
1
1
NIL
HORIZONTAL

SLIDER
275
378
463
411
prob-revision
prob-revision
0.001
1
0.1
0.001
1
NIL
HORIZONTAL

SLIDER
557
607
706
640
prob-mutation
prob-mutation
0
1
0.0
0.001
1
NIL
HORIZONTAL

BUTTON
26
10
110
43
setup
startup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
222
10
302
43
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
121
10
210
43
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
678
10
798
55
NIL
ticks
17
1
11

PLOT
25
60
466
304
Strategy distributions (recent history)
milliseconds
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS

PLOT
475
60
931
304
Strategy distributions (complete history)
seconds
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS

SLIDER
280
528
475
561
duration-of-recent
duration-of-recent
1
100
10.0
1
1
sec.
HORIZONTAL

SWITCH
281
567
475
600
show-recent-history?
show-recent-history?
0
1
-1000

SWITCH
282
607
475
640
show-complete-history?
show-complete-history?
0
1
-1000

INPUTBOX
27
581
232
641
initial-condition
[260 20 20]
1
0
String (reporter)

SWITCH
275
341
463
374
use-prob-revision?
use-prob-revision?
1
1
-1000

SLIDER
280
488
475
521
plot-every-?-secs
plot-every-?-secs
0.01
5
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
276
415
463
448
n-of-revisions-per-tick
n-of-revisions-per-tick
1
n-of-agents
30.0
1
1
NIL
HORIZONTAL

MONITOR
809
10
930
55
NIL
ticks-per-second
3
1
11

SWITCH
742
341
924
374
complete-matching?
complete-matching?
1
1
-1000

SLIDER
741
394
924
427
n-of-trials
n-of-trials
1
10
2.0
1
1
NIL
HORIZONTAL

SLIDER
521
407
688
440
n-in-test-set
n-in-test-set
2
max-size-of-test-set
3.0
1
1
NIL
HORIZONTAL

TEXTBOX
722
600
777
618
for logit:
11
0.0
1

SLIDER
722
616
873
649
eta
eta
0.001
5
0.001
0.001
1
NIL
HORIZONTAL

CHOOSER
720
553
872
598
tie-breaker
tie-breaker
"stick-uniform" "stick-min" "uniform" "min" "random-walk"
2

TEXTBOX
722
536
895
554
for best:
11
0.0
1

SLIDER
522
461
687
494
n-to-consider-imitating
n-to-consider-imitating
1
max-number-to-consider-imitating
2.0
1
1
NIL
HORIZONTAL

TEXTBOX
523
444
704
463
for imitative & (best or logit):
11
0.0
1

TEXTBOX
523
391
674
409
for direct & (best or logit):
11
0.0
1

SLIDER
23
857
231
890
random-walk-speed
random-walk-speed
0
1
1.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
24
842
284
861
for best & random-walk tie-breaker:
11
0.0
1

CHOOSER
520
340
687
385
candidate-selection
candidate-selection
"imitative" "direct"
1

CHOOSER
557
539
708
584
decision-method
decision-method
"best" "logit" "proportional"
0

SWITCH
27
541
230
574
random-initial-condition?
random-initial-condition?
1
1
-1000

TEXTBOX
743
431
953
461
for complete-matching=off \n     & direct:
11
0.0
1

SWITCH
743
461
924
494
single-sample?
single-sample?
0
1
-1000

SWITCH
22
715
228
748
trials-with-replacement?
trials-with-replacement?
1
1
-1000

SWITCH
23
767
239
800
imitatees-with-replacement?
imitatees-with-replacement?
0
1
-1000

SWITCH
22
663
205
696
self-matching?
self-matching?
0
1
-1000

SWITCH
23
805
240
838
consider-imitating-self?
consider-imitating-self?
0
1
-1000

PLOT
275
663
593
890
Strategies' expected payoff (recent history)
milliseconds
NIL
0.0
1.0
0.0
0.0
true
true
"" ""
PENS

PLOT
598
663
935
890
Strategies' expected payoff (complete history)
seconds
NIL
0.0
1.0
0.0
0.0
true
true
"" ""
PENS

BUTTON
330
10
490
43
load parameters from file
load-parameter-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
500
10
647
43
save parameters to file
save-parameter-file
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
23
752
214
770
for imitative:\n
11
0.0
1

TEXTBOX
743
378
910
396
for complete-matching=off:
11
0.0
1

TEXTBOX
254
319
496
351
Assignment of revision opportunities
13
13.0
1

TEXTBOX
804
320
877
338
Matching
13
13.0
1

TEXTBOX
541
319
691
337
Candidate selection
13
13.0
1

TEXTBOX
557
591
707
609
mutations:
11
0.0
1

TEXTBOX
654
515
784
533
Decision method
13
13.0
1

TEXTBOX
62
318
212
336
Game and population
13
13.0
1

TEXTBOX
320
468
438
486
Plotting of output
12
0.0
1

TEXTBOX
24
700
205
718
for complete-matching=off:
11
0.0
1

TEXTBOX
23
646
261
664
---------------------------------------\n
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
