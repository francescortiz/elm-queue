module Queue exposing
    ( Queue, empty, enqueueAndStart, start, enqueue, enqueueMany, dequeue
    , QueueUnkeyed, emptyUnkeyed, startUnkeyed, enqueueUnkeyed, enqueueManyUnkeyed
    )

{-| Queues for parallel processing or rate limiting. They are totally type agnostic, soy they serve general use case. You
can enqueue any elm type, even Cmd or Msg.


# Keyed queues

By providing a key you prevent duplicate entries in the queue. It also allows to remove specific entries.

@docs Queue, empty, enqueueAndStart, start, enqueue, enqueueMany, dequeue


# Unkeyed queues

Simple lists of tasks, duplication allowed. Items cannot be removed.

@docs QueueUnkeyed, emptyUnkeyed, startUnkeyed, enqueueUnkeyed, enqueueManyUnkeyed

-}

import List.Extra


type alias Lane a =
    ( String, a )


type alias QueueBody a =
    { backlog : List (Lane a)
    , activeLanes : List (Lane a)
    , poolSize : Int
    }


{-| Keyed queue. Keyed queues don't have duplicates. You have to dequeue items that have in started in order to free up
space in the queue pool.
-}
type Queue a
    = Queue (QueueBody a)


{-| Created empty Queue. You have to specify the size of pool (or parallelization count).
-}
empty : Int -> Queue a
empty poolSize =
    Queue
        { backlog = []
        , activeLanes = []
        , poolSize = poolSize
        }


{-| Add entry to the queue and start processing it (get first elements to start processing them).
-}
enqueueAndStart : String -> a -> Queue a -> ( Queue a, List a )
enqueueAndStart key item queue =
    queue
        |> enqueue key item
        |> start


{-| Start processing the queue (get first elements to start processing them). You need to call dequeue on complete items
in order to free up pool space.
-}
start : Queue a -> ( Queue a, List a )
start (Queue queue) =
    let
        poolSize =
            queue.poolSize

        availableLanes =
            queue.activeLanes
                |> List.length
                |> (\l_ -> poolSize - l_)
                |> max 0

        ( newLanes, newBacklog ) =
            queue.backlog
                |> List.reverse
                |> List.Extra.splitAt availableLanes
                |> (\( a, b ) -> ( a, List.reverse b ))
    in
    ( Queue
        { queue
            | activeLanes =
                queue.activeLanes
                    |> List.append newLanes
            , backlog =
                newBacklog
        }
    , newLanes
        |> List.map Tuple.second
    )


{-| Add one element to the queue. You have to provide a key be able to track the item completion.

    queue
        |> enqueue "get-user-photo" httpRequest
        |> start

-}
enqueue : String -> a -> Queue a -> Queue a
enqueue key item (Queue queue) =
    if not (alreadyEnqueued key queue) then
        Queue
            { queue
                | backlog = ( key, item ) :: queue.backlog
            }

    else
        Queue queue


{-| Add many elements to the queue.
-}
enqueueMany : List ( String, a ) -> Queue a -> Queue a
enqueueMany keyedItems queue =
    keyedItems
        |> List.foldl
            (\( key, item ) queue_ ->
                queue_
                    |> enqueue key item
            )
            queue


alreadyEnqueued key queue =
    List.any (\( key_, _ ) -> key == key_) queue.backlog
        || List.any (\( key_, _ ) -> key == key_) queue.activeLanes


{-| Mark an element of the queue as complete. You have to provide the element key. This also frees up space in the pool.

    queue
        |> dequeue itemKey
        |> start

-}
dequeue : String -> Queue a -> Queue a
dequeue key (Queue queue) =
    let
        matchedLaneIndex =
            queue.activeLanes
                |> List.Extra.findIndex (\( key_, item ) -> key == key_)
    in
    case matchedLaneIndex of
        Just laneIndex ->
            queue
                |> clearActiveLane laneIndex
                |> Queue

        Nothing ->
            queue
                |> Queue


clearActiveLane : Int -> QueueBody a -> QueueBody a
clearActiveLane laneIndex queue =
    { queue
        | activeLanes =
            queue.activeLanes
                |> List.Extra.removeAt laneIndex
    }


activateNextTract : QueueBody a -> QueueBody a
activateNextTract queue =
    let
        ( nextLane, newBacklog ) =
            case queue.backlog of
                [] ->
                    ( Nothing, [] )

                x :: xs ->
                    ( Just x, xs )
    in
    case nextLane of
        Just lane ->
            { queue
                | activeLanes = lane :: queue.activeLanes
                , backlog = newBacklog
            }

        Nothing ->
            queue


{-| Body of QueueUnkeyed.
-}
type alias QueueUnkeyedBody a =
    { backlog : List a
    , activeLanes : List a
    , poolSize : Int
    }


{-| Queue that does not care about when a task is finished.
-}
type QueueUnkeyed a
    = QueueUnkeyed (QueueUnkeyedBody a)


{-| Create an empty unkeyed queue. You have to provide an integer value that states the number of items retrieved on
each iteration.

    init =
        ( { msgQueue = emptyUnkeyed 10
          }
        , Cmd.none
        )

-}
emptyUnkeyed : Int -> QueueUnkeyed a
emptyUnkeyed poolSize =
    QueueUnkeyed
        { backlog = []
        , activeLanes = []
        , poolSize = poolSize
        }


{-| Start processing the queue (get first elements to start processing them). Started elements don't belong to the queue anymore.

    startUnkeyed model.msgQueue

-}
startUnkeyed : QueueUnkeyed a -> ( QueueUnkeyed a, List a )
startUnkeyed (QueueUnkeyed queue) =
    let
        poolSize =
            queue.poolSize

        availableLanes =
            queue.activeLanes
                |> List.length
                |> (\l_ -> poolSize - l_)
                |> max 0

        ( newLanes, newBacklog ) =
            queue.backlog
                |> List.reverse
                |> List.Extra.splitAt availableLanes
                |> (\( a, b ) -> ( a, List.reverse b ))
    in
    ( QueueUnkeyed
        { queue
            | backlog =
                newBacklog
        }
    , newLanes
    )


{-| Enqueue an item to an unkeyed queue. It allows to enqueue repeated elements.

    enqueueUnkeyed (ItemMsg item) model.msgQueue

-}
enqueueUnkeyed : a -> QueueUnkeyed a -> QueueUnkeyed a
enqueueUnkeyed item (QueueUnkeyed queue) =
    QueueUnkeyed
        { queue
            | backlog = item :: queue.backlog
        }


{-| Enqueue a list of items to an unkeyed queue queue.

    enqueueManyUnkeyed [ ItemMsg item1, ItemOtherMsg item1, ItemMsg item2 ] model.msgQueue

-}
enqueueManyUnkeyed : List a -> QueueUnkeyed a -> QueueUnkeyed a
enqueueManyUnkeyed items queue =
    items
        |> List.foldl
            (\item queue_ ->
                queue_
                    |> enqueueUnkeyed item
            )
            queue
