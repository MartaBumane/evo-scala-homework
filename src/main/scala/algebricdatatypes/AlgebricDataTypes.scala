package algebricdatatypes

object AlgebraicDataTypes {
    sealed trait Suite
    object Suite {
        final case object H extends Suite
        final case object D extends Suite
        final case object C extends Suite
        final case object S extends Suite
    }

    sealed trait Rank {
        def symbol: Char
        def value: Int
    }
    object Rank {
        case object Two extends Rank {
            val symbol = '2'
            val value = 2
        }
        
        case object Three extends Rank {
            val symbol = '3'
            val value = 3
        }
        
        case object Four extends Rank {
            val symbol = '4'
            val value = 4
        }
        
        case object Five extends Rank {
            val symbol = '5'
            val value = 5
        }
        
        case object Six extends Rank {
            val symbol = '6'
            val value = 6
        }
        
        case object Seven extends Rank {
            val symbol = '7'
            val value = 7
        }
        
        case object Eight extends Rank {
            val symbol = '8'
            val value = 8
        }
        
        case object Nine extends Rank {
            val symbol = '9'
            val value = 9
        }
        
        case object Ten extends Rank {
            val symbol = 'T'
            val value = 10
        }
        
        case object Jack extends Rank {
            val symbol = 'J'
            val value = 11
        }
        
        case object Queen extends Rank {
            val symbol = 'Q'
            val value = 12
        }
        
        case object King extends Rank {
            val symbol = 'K'
            val value = 13
        }
        
        case object Ace extends Rank {
            val symbol = 'A'
            val value = 14
        }
    }

    final case class Card(suite: Suite, rank: Rank)

    final case class Hand private (handCards: Set[Card])
    object Hand {
        def create(cards: List[Card]): Option[Hand] = {
            handCards.length match {
                case 2 => Some(Hand(cards))
                case _ => None
            }
        }
    }

    final case class Board private (boardCards: List[Card])
    object Board {
        def create(boardCards: List[Card]): Option[Board] = {
            boardCards match {
                case list if list.length == 5 => Some(Board(list))
                case _                  => None
            }
        }
    }

    sealed trait Combination
    object Combination {
        case class RoyalStraightFlush(cards: List[Card], hand: Hand) extends Combination {
            var rank = 10
            val combinationCards: List[Card] = cards
            val handCards: Hand = hand
        }
        case class StraightFlush(cards: List[Card], hand: Hand) extends Combination {
            var rank = 9
            val combinationCards: List[Card] = cards
            val handCards: Hand = hand
        }
        case class FourOfAKind(cards: List[Card], hand: Hand) extends Combination {
            var rank = 8
            val combinationCards: List[Card] = cards
            val handCards: Hand = hand
        }
        case class FullHouse(cards: List[Card], hand: Hand) extends Combination {
            var rank = 7
            val combinationCards: List[Card] = cards
            val handCards: Hand = hand   
        }
        case class Flash(cards: List[Card], hand: Hand) extends Combination {
            var rank = 6
            val combinationCards: List[Card] = cards
            val handCards: Hand = hand
        }
        case class Straight(cards: List[Card], hand: Hand) extends Combination {
            var rank = 5
            val combinationCards: List[Card] = cards
            val handCards: Hand = hand
        }
        case class ThreeOfAKind(cards: List[Card], hand: Hand) extends Combination {
            var rank = 4
            val combinationCards: List[Card] = cards
            val handCards: Hand = hand
        }
        case class TwoPair(cards: List[Card], hand: Hand) extends Combination {
            var rank = 3
            val combinationCards: List[Card] = cards
            val handCards: Hand = hand
        }
        case class Pair(cards: List[Card], hand: Hand) extends Combination {
            var rank = 2
            val combinationCards: List[Card] = cards
            val handCards: Hand = hand
        }
        case class HigherCard(cards: List[Card], hand: Hand) extends Combination {
            var rank = 1
            val combinationCards: List[Card] = cards
            val handCards: Hand = hand
        }
    }

    final case class TestCase(board: Board, hands: Set[Hand])
    object TestCase {
        def create(
            board: Board,
            hands: List[Hand],
        ): Option[TestCase] = 
        if (hands.length >= 2)  Some(TestCase(board, hands))
        else None
    }

    case class PlayerCombination (hand: Hand, combination: Combination)

    case class TestResult (handsSortedByRank: List[PlayerCombination]) extends AnyVal
    object TestResult {
        def create (handsSortedByRank: List[PlayerCombination]): Option[TestResult] =
        if (handsSortedByRank.length >= 2) Some(TestResult(handsSortedByRank))
        else None
    }
}
