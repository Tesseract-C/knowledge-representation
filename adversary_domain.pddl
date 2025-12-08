(define (domain adversary_domain)

    (:requirements :strips :typing)

    (:types
        location - object
        chaser thief player - object
    )

    (:predicates
        (at ?obj - object ?loc - location)
        (link ?from - location ?to - location)
        (player-at ?loc - location)
        (has-amulet ?obj - object)
        (caught-player)
    )

    ; Move chaser from one location to another
    (:action move-chaser

        :parameters (?chaser - chaser ?from ?to - location)

        :precondition (and
            (at ?chaser ?from)
            (link ?from ?to)
        )

        :effect (and
            (not (at ?chaser ?from))
            (at ?chaser ?to)
        )
    )

    ; Move thief from one location to another
    (:action move-thief

        :parameters (?thief - thief ?from ?to - location)

        :precondition (and
            (at ?thief ?from)
            (link ?from ?to)
        )

        :effect (and
            (not (at ?thief ?from))
            (at ?thief ?to)
        )
    )

    ; Catch player action - only for chaser
    (:action catch-player

        :parameters (?chaser - chaser ?loc - location)

        :precondition (and
            (at ?chaser ?loc)
            (at player ?loc)
        )

        :effect (and
            (not (at player ?loc))
            (caught-player)
        )
    )

    ; Steal item from player - only for thief
    (:action steal-item

        :parameters (?thief - thief ?player-loc - location)

        :precondition (and
            (at ?thief ?player-loc)
            (at player ?player-loc)
        )

        :effect (and
            (not (has-amulet player))
            (has-amulet ?thief)
        )
    )

    ; Coordination action - chaser blocks escape route
    (:action block-escape

        :parameters (?chaser - chaser ?player-loc - location ?escape-loc - location)

        :precondition (and
            (at ?chaser ?escape-loc)
            (at player ?player-loc)
            (link ?player-loc ?escape-loc)
        )

        :effect (and
            (at ?chaser ?escape-loc)
        )
    )
)