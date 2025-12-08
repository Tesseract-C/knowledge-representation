(define (problem adversary_problem_1)

    (:domain adversary_domain)

    (:objects
        entrance hall armory throne pit - location
        chaser - chaser
        thief - thief
        player - player
    )

    (:init
        ; Define the links between locations
        (link entrance hall)
        (link hall entrance)
        (link hall armory)
        (link armory hall)
        (link hall throne)
        (link throne hall)
        (link hall pit)
        (link pit hall)

        ; Initial positions
        (at chaser throne)
        (at thief entrance)
        (at player entrance)
        (player-at entrance)
        ; Player initially has no amulet
        (not (has-amulet player))
    )

    (:goal
        (and
            ; Goal: catch the player
            (caught-player)
        )
    )
)