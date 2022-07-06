;; rewards.clar
;; 

;; constants
;;
(define-constant ERR-UNAUTHORIZED  (err u201))
(define-constant ERR-JOB-DOESNT-EXIST  (err u202))
(define-constant ERR-INVALID-CLAIM (err u203))
(define-constant ERR-REWARDS-PAID (err u204))

(define-constant contract-principal (as-contract tx-sender))
(define-constant contract-owner tx-sender)

(define-constant DAO-address 'STNHKEPYEPJ8ET55ZZ0M5A34J0R3N5FM2CMMMAZ6) ;; walllet 9
(define-constant charity-address 'ST3PF13W7Z0RRM42A8VZRVFQ75SV1K26RXEP8YGKJ );; wallet 7



;; data maps and vars
;;

(define-data-var DAO-percentage uint u150)
(define-map bounties { job-id: (string-ascii 10), wallet-address: principal } { reward-amount: uint} )

;; private functions
;;

(define-private (transfer-funds (amount uint) (sender principal) (receiver principal))

(begin

    (try! (contract-call? 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.vibes-token transfer amount sender receiver none) ) 
    (ok true)
)
)

;; public functions
;;

(define-public (pay-rewards (job-id (string-ascii 10)) (hire principal) (referrer (optional principal)) (yearly-sal uint) (bounty-percentage uint) (charity-percentage uint))
(begin 
    (asserts! (is-eq (is-valid-bounty job-id hire) false) ERR-REWARDS-PAID)
    (let 
        (
            (vibes-yearly-sal (* yearly-sal u100000000))

            (total-bounty (/ (* vibes-yearly-sal bounty-percentage) u10000))

            (hire-bounty
                (if (is-some referrer) 
                    ;;true
                    (/ total-bounty u2)
                    ;; false
                    total-bounty
                )
            )

            (referrer-bounty
                (if (is-some referrer) 
                    ;;true
                    (/ total-bounty u2)
                    ;; false
                    u0
                )
            )

            (charity-amount  (/ (* vibes-yearly-sal charity-percentage) u10000))
            (DAO-amount (/ (* vibes-yearly-sal (var-get DAO-percentage)) u10000) )
        )

        (print charity-amount)
        (print DAO-amount)
        (print hire-bounty)
        (print referrer-bounty)
        (try! (transfer-funds charity-amount tx-sender charity-address))
        (try! (transfer-funds DAO-amount tx-sender DAO-address))
        

        (if (is-some referrer)
            ;;true
            (begin
                (try! (transfer-funds hire-bounty tx-sender contract-principal))
                (try! (transfer-funds referrer-bounty tx-sender  contract-principal)) 
                (map-set bounties {job-id: job-id, wallet-address: hire} {reward-amount: hire-bounty})
                (map-set bounties {job-id: job-id, wallet-address: (unwrap-panic referrer)} {reward-amount: referrer-bounty})
            )
            ;; false
            (begin 
                (try! (transfer-funds hire-bounty tx-sender contract-principal)) 
                (map-set bounties {job-id: job-id, wallet-address: hire} {reward-amount: hire-bounty})
            )
        )

        (ok true)

    )
)
)

(define-public (claim-bounty (job-id (string-ascii 10)) (wallet-address principal)) 
(begin
    (asserts! (is-valid-bounty job-id wallet-address) ERR-INVALID-CLAIM)
    (let 
        (
            (bounty-amount (get reward-amount (unwrap! (map-get? bounties {job-id: job-id, wallet-address: wallet-address}) (err u205) )) )
        ) 

        (try! (as-contract (transfer-funds bounty-amount tx-sender wallet-address)))
        (map-delete bounties {job-id: job-id, wallet-address: wallet-address})
        (ok true)
    )

)
)

(define-read-only (is-valid-bounty (job-id (string-ascii 10)) (wallet-address principal)) 
    
    (if (is-some (map-get? bounties {job-id: job-id, wallet-address: wallet-address}))
        ;;true
        true
        ;;false
        false
    )
)

(define-public (set-DAO-percentage (val uint))
(begin

    (asserts! (is-eq tx-sender contract-owner) ERR-UNAUTHORIZED)

    (var-set  DAO-percentage val)
    (ok true)

)
)

;; read only functions

(define-read-only (get-contract-owner) 
    contract-owner
)

(define-read-only (get-contract-principal) 
    contract-principal
)



