(define (problem current_adversary_problem)
    (:domain adversary_domain)

    (:objects
        entrance hall armory throne pit - location
        chaser thief - adversary
        player - player
    )

    (:init
        (link entrance hall)
        (link hall entrance)
        (link hall armory)
        (link armory hall)
        (link hall throne)
        (link throne hall)
        (link hall pit)
        (link pit hall)
        (at chaser throne)
        (at thief pit)
        (at player entrance)
        (player-at entrance)
    )

    (:goal
        (and
            (at chaser throne)
        )
    )
)
