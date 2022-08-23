// ########################
// ####  Requirements  ####
// ########################
// 
// Design an API for a Tic-Tac-Toe board, consisting of types
// representing states of the board, along with functions 
// move, takeMoveBack, whoWonOrDraw, and isPositionOccupied.
// 
// * All functions must be pure
// * All functions must return a sensible value and may not throw exceptions
// * A move can only be made if
//   - the game is not over
//   - the player is the current player
//   - the move is valid (i.e. not already played)
// * Calling TakeMoveBack on a board with no moves is a compile time error
// * Calling WhoWonOrDraw on a tic-tac-toe board but the game has not finished is a compile time error
// * IsPositionOccupied works for in-play and completed games.

// #################
// ####  Utils  ####
// #################
//
// CartesianProduct<X,Y>
// Example: 
// CartesianProduct<"a" | "b", "c" | "d"> = ["a,c", "a,d", "b,c", "b,d"]
type CartesianProductString<T extends ToStringableTypes,T2 extends ToStringableTypes > = `${T}${T2}`;
type ToStringableTypes = string | number | boolean | bigint;

// UnionToIntersection<X>
// Takes a union like `A | B | C` and returns an intersection like `A & B & C`
type UnionToIntersection<U> = 
  (U extends any ? (k: U)=>void : never) extends ((k: infer I)=>void) ? I : never

// Example: ToUnion<[1,2,3]> = 1 | 2 | 3
type ToUnion<T extends any[]> = T[number]

// Zip two arrays together
// Example: Zip<[1,2,3],["a","b","c"]> = [[1,"a"],[2,"b"],[3,"c"]]
type Zip<T extends any[], U extends any[], Acc extends any[] = []> = 
  T extends [infer Head, ...infer Tail] ? 
  U extends [infer Head2, ...infer Tail2] ? 
  Zip<Tail, Tail2, [...Acc, [Head, Head2]]> : Acc  : Acc;


// Join tuples together to strings
// Example: StringConcatTuples<[[1,2],[3,4]]> = ["12","34"]
type StringConcatTuples<T extends [number, number][]> = {[Key in keyof T]: `${T[Key][0]}${T[Key][1]}`};

// ####  Math Utils  ####

// MinusOne<N>
// Defined between 1 and 1000
// Take a number N
// Check if length of empty array + one unknown element is equal to N
// If it is return length of array which is one less then N.
// If not recursively call MinusOne with an array that is one element longer.
type MinusOne<N extends number, Arr extends any[] = []> = [
  ...Arr,
  unknown
]['length'] extends N
  ? Arr['length']
  : MinusOne<N, [...Arr, unknown]>

// PlusOne<N>
// Defined between 1 and 1000
// Same idea as above but when we hit our base case
// Add two extra elements to the array and return the length of the array.
// This way we get N + 1
type PlusOne<N extends number, Arr extends any[] = []> = 
  [...Arr, unknown]['length'] extends N
    ? [...Arr, unknown, unknown]['length']
    : PlusOne<N, [...Arr, unknown]>

// FromToInc<Lower,Higher>
// Gives back an Array of all numbers between Lower and Higher (inclusive)
// Example: FromToInc<1,3> = [1,2,3]
type FromToInc<From extends number, To extends number, acc extends any[] = []> = From extends PlusOne<To> ? acc : FromToInc<PlusOne<From>, To, [...acc, From]>;

// FromToDec<Higher,Lower>
// Gives back an Array of all numbers between Higher and Lower (inclusive)
// Example: FromToDec<3,1> = [3,2,1]
type FromToDec<From extends number, To extends number, acc extends any[] = []> = From extends MinusOne<To> ? acc : FromToDec<MinusOne<From>, To, [...acc, From]>;


// #####################
// ####  Data Types ####
// #####################

interface Circle { __type: "O";     }
interface Cross  { __type: "X";     }
interface Empty  { __type: "Empty"; }
interface Nil    { __type: "Nil";   }

type Player = Cross | Circle;
type Square = Player | Empty;


// ##############################################
// ##############################################
// #### SELECT THE SIZE OF THE GAME YOU WANT ####
// ##############################################
// ##############################################
type Size = 3
// ##############################################
// ##############################################
// In a future version of the game, types will 
// be parameterized by the size of the game.
// So multiple games of different sizes can 
// exist at the same time.
// 
// However even now everything is calculated from the Size
// So 4x4 games are possible
// ##############################################

// ###################################
// ####    Column/Row/Position    ####
// ###################################
// Column and Row can potentially be different sizes
// Though winning on the diagonal will have to change 
// If the game is not square.
type Column      = ToUnion<FromToInc<1,Size>>;
type Row         = ToUnion<FromToInc<1,Size>>;
type Coordinates = CartesianProductString<Column, Row>


// ###################################
// ####     Winning Positions     ####
// ###################################
// The way we are going to see if a player has won yet
// is by using the fact that we know what combination of
// positions are winning, if a player has all of them they have won.
// Steps:
//
// 1. Create a list of all the winning positions
//    ["11", "22","33"] 
//
// 2. Check what is the state of all of them
//    [Circle, Cross, Empty]
//
// 3. Create an intersection of all the states
//    Circle & Cross & Empty
//    never
//
// Only time we get a value is when all three are the same
//   [Circle, Circle, Circle]
//   Circle & Circle & Circle
//   Circle
//
// Now we can check if the player is the winner.
//
// ###################################
// #### Winning Positions Helpers ####
// ###################################
type GetRowPositions<C extends Column, R extends Row> = 
    R extends Row 
      ? [CartesianProductString<C, R>] 
      : never;

type GetColumnPositions<C extends Column, R extends Row> = 
    C extends Column 
      ? [CartesianProductString<C, R>] 
      : never;


// Only first type parameter is allowed to be supplied
type Diagonals<Size extends number, 
               _FromToSize extends number[] = FromToInc<1,Size>, 
               _SizeToFrom extends number[] = FromToDec<Size, 1>, 
               > =  
  | Zip<_FromToSize, _FromToSize> 
  | Zip<_FromToSize, _SizeToFrom>

type GetDiagonalPositions<S extends Size> = StringConcatTuples<Diagonals<S>>

// WinningPositions
// Example
// [["1,1", "2,2", "3,3"], ["1,3", "2,2", "3,1"] ... ]
// Used to check if a player has won, by using Positions 
// to look up the current state of the squares.
type WinningPositions =
  // Rows
  | GetRowPositions<Column, Row>
  // Columns
  | GetColumnPositions<Column, Row>
  // Diagonals
  | GetDiagonalPositions<Size>

// ###################################
// ####          Squares          ####
// ###################################
//
// Fundamental data type of the game.
// Board contains a mapping from Coordinate to Square
// A Square is either Empty, Cross, or Circle.
//
type Board = { [s in Coordinates]: Square };

// ###################################
// ####        Get Winner         ####
// ###################################
// 
// LookupPosition returns the state of the squares at each position listed
// ["11", "12", "13"] -> [Circle, Circle, Circle]
type LookupCoordinates<Coords extends Array<Coordinates>, S extends Board> =
  { [Key in keyof Coords ]: S[Coords[Key]] }

// If there is any Array that only contains the same element
// Then that element(s) will be returned
type UniqueInSequence<P extends Array<unknown>> =
  P extends Array<unknown> ? UnionToIntersection<P[number]> : never

// If there is a winner in the Squares provided then the winner is returned.
type GetWinnerOrNever<S extends Board> =
   UniqueInSequence<LookupCoordinates<WinningPositions,S>>


// ###################################
// ####           Board           ####
// ###################################
// 
// A Round has a bunch of Squares and a Player that is next to move.
// It also has a previous Round or Nil, to be able to allow for undoing moves.
interface Round<
  S extends Board,
  P extends Player,
  B extends Round<any, any, any> | Nil
> extends HasPrevious<B> {
  __tag: "round";
  squares: S;
  nextToMove: P;
}

interface HasPrevious<P> {
  previous: P;
}

// ###################################
// ####    Initial game states    ####
// ###################################
//
type InitialBoard = { [key in keyof Board]: Empty };
type InitialRound = Round<InitialBoard, Cross, Nil>;

// Squares that are possible to play on
type AvailableSquares<B extends Board> = {
  [key in keyof B]: B[key] extends Empty ? key : never;
}[keyof B];



// ###################################
// ####        Game States        ####
// ###################################
//
interface Winner<
  S extends Player,
  PrevR extends Round<any, any, any>,
  Curr extends Board
> extends HasPrevious<PrevR> {
  __tag: "winner",
  winningPosition: Curr;
  winner: S;
}

interface Draw<R extends Round<any, any, any>> extends HasPrevious<R> {__tag: "draw";}

// ###################################
// ####        Game Actions       ####
// ###################################
//
// ######################
// #######  Move  #######
// ######################
// 
// We check if the move is valid with the type constraint AvailableSquares
// Then we apply that move and see if the next Round has a Winner.
// If it does then we return the Winner.
// If Not we check if it's the end of the game with NoMoreSquares.
// If it is we return a Draw.
// If not we return the next Round.
//
// There is some duplication of the next round
// This could be solved with more type parameters of the form
// <..., Name = DefaultValue>
// This does however open the door for the user to provide a value that might 
// Circumvent the checks done so duplication it is.
type Move<
  CurrentRound extends Round<Board, P, any>,
  P extends Player,
  Position extends AvailableSquares<CurrentRound["squares"]>
> = HasWon<
  P,
  Round<SetSquare<CurrentRound["squares"], Position, P>, GetNextPlayer<P>, CurrentRound>
> extends true
  ? Winner<P, CurrentRound, SetSquare<CurrentRound["squares"], Position, P>>
  : NoMoreSquares<SetSquare<CurrentRound["squares"], Position, P>> extends true
    ? Draw<CurrentRound>
    : Round<
        SetSquare<CurrentRound["squares"], Position, P>,
        GetNextPlayer<P>,
        CurrentRound
      >;

// ##########################
// ######  Move Utils  ######
// ##########################
//
type GetNextPlayer<P extends Player> = P extends Cross ? Circle : Cross;

// Sets a Square to a Player
type SetSquare<Sqs extends Board, PositionToSet, Player> = {
  [Pos in keyof Sqs]: Pos extends PositionToSet ? Player : Sqs[Pos];
};

// Checks if there is no more squares to play on.
type NoMoreSquares<S extends Board> = AvailableSquares<S> extends never
  ? true
  : false;

// Checks if a player has won.
type HasWon<
  P extends Player,
  B extends Round<any, any, any>
> = P extends GetWinnerOrNever<B["squares"]> ? true : false;

 
// ###########################################
// ####        Game State Functions       ####
// ###########################################

type WhoWonOrDraw<A extends Draw<any> | Winner<Player, any, any>> = (
  state: A
) => A extends Winner<infer P, any, any>
  ? PlayerWinnerString<P>
  : "The game was a draw";

// Util
type PlayerWinnerString<P extends Player> = P extends Circle
  ? "Circle Won the game"
  : "Cross Won the game";

type TakeMoveBack<B extends HasPrevious<Round<any, any, any>>> = B["previous"];

//  
// We check if there if the Square is Empty in the Position provided. 
// Depending on the game state we look at different fields. 
// We return a boolean to indicate if the square is occupied or not.
// We don't allow the function to be called with Draw, because a Draw game
// has by definition no squares to play on.
type IsPositionOccupied<
  RW extends Winner<any, any, any> | Round<any, any, any>,
  Pos extends Coordinates
> = (
  RW extends Winner<any, any, any>
    ? RW["winningPosition"][Pos]
    : RW extends Round<any, any, any>
    ? RW["squares"][Pos]
    : never
) extends Empty
  ? false
  : true;



// ###################################
// ####         TEST Cases        ####
// ###################################
//
// Note, since these are type-level tests 
// we want to check for type errors while we want the program to compile
// Typescript gives us this handy comment we can use to check for type errors.
// \@ts-expect-error 
// This allows us to get errors if we don't get type errors.

// ######################
// ####  Test Utils  ####
// ######################
// 
// We need some type-level utility functions to help us with the game.
// Equal and Expect functions have been taken from the excellent type-challenges repo.
// https://github.com/type-challenges/type-challenges/blob/master/utils/index.d.ts
// 

//  Equal<X,Y> 
//  Check if two types are equal.
//  ------------------------------------------------------------
//  For full discussion around this type, see:
//  https://github.com/Microsoft/TypeScript/issues/27024#issuecomment-421529650
// 
// > ...It relies on conditional types being deferred when T is not known. 
// > Assignability of deferred conditional types relies on an internal isTypeIdenticalTo check, 
// > which is only true for two conditional types if:
// > 
// >   * Both conditional types have the same constraint
// >   * The true and false branches of both conditions are the same type
// > - https://github.com/Microsoft/TypeScript/issues/27024#issuecomment-510924206
// 

type Equal<X, Y> = 
  (<T>() => T extends X ? 1 : 2) extends 
  (<T>() => T extends Y ? 1 : 2) ? true : false;

// Expect<T>
// Give type-level error if T is not true
type Expect<T extends true> = T;


type CircleWonString = ReturnType<WhoWonOrDraw<WinCircleFinal>>;
type CrossWonString  = ReturnType<WhoWonOrDraw<WinCrossFinal>>;
type DrawString      = ReturnType<WhoWonOrDraw<DrawFinal>>;

// ##################################
// #              TEST              #
// ##################################
// #         No double moves        #
// ##################################
// @ts-expect-error
type NoDoubleMove = Move<Move<InitialRound, Cross, "33">, Cross, "13" >;

// ##################################
// #              TEST              #
// ##################################
// #      No start with Circle      #
// ##################################
// @ts-expect-error
type NoStartWithCircle = Move<InitialRound, Circle, "33">;


// ##################################
// #              TEST              #
// ##################################
// #      No taking a used square   #
// ##################################
// @ts-expect-error
type NoTakingAUsedSquare = Move<Move<InitialRound, Cross, "33">,Circle, "33">

// ##################################
// #              TEST              #
// ##################################
// #  InitialRound has no previous  #
// #  Don't allow to take too many  #
// #           moves back           #
// ##################################
// @ts-expect-error
type NotToManyMovesBack = TakeMoveBack<InitialRound>;


// ##################################
// #              TEST              #
// ##################################
// #         Making a move          #
// #       and taking it back       #
// #   Gives back the start state   #
// ##################################
type MoveOne      = Move<InitialRound, Cross, "33">;
type BackToNormal = TakeMoveBack<MoveOne>;
type Outcome      = Expect<Equal<InitialRound, BackToNormal>>;

type BackFromDraw = Expect<Equal<TakeMoveBack<DrawFinal>, DrawStep8>>;
type BackFromWin = Expect<Equal<TakeMoveBack<WinCrossFinal>, WinCrossStep4>>;

// ##################################
// #              TEST              #
// ##################################
// #       IsPositionOccupied       #
// ##################################
type Test = Expect<
  Equal<IsPositionOccupied<WinCrossFinal, "12">, false>
>;
type Test2 = Expect<Equal<IsPositionOccupied<WinCrossFinal, "33">, true>>;
type Test3 = Expect<Equal<IsPositionOccupied<InitialRound, "33">, false>>;
// @ts-expect-error
type Test4 = Expect<Equal<IsPositionOccupied<InitialRound, "33">, true>>;


// ##################################
// #              TEST              #
// ##################################
// #    GAME That ends in a draw    #
// ##################################
type DrawStep1 = Move<InitialRound, Cross, "13">;
type DrawStep2 = Move<DrawStep1, Circle, "23">;
type DrawStep3 = Move<DrawStep2, Cross, "33">;
type DrawStep4 = Move<DrawStep3, Circle, "12">;
type DrawStep5 = Move<DrawStep4, Cross, "22">;
type DrawStep6 = Move<DrawStep5, Circle, "11">;
type DrawStep7 = Move<DrawStep6, Cross, "32">;
type DrawStep8 = Move<DrawStep7, Circle, "31">;
type DrawFinal = Move<DrawStep8, Cross, "21">;
type DrawOutcome = Expect<Draw<any> extends DrawFinal ? true : false>;

// ##################################
// #              TEST              #
// ##################################
// #     GAME That Cross Wins       #
// ##################################
type WinCrossStep1 = Move<InitialRound, Cross, "13">;
type WinCrossStep2 = Move<WinCrossStep1, Circle, "11">;
type WinCrossStep3 = Move<WinCrossStep2, Cross, "33">;
type WinCrossStep4 = Move<WinCrossStep3, Circle, "21">;
type WinCrossFinal = Move<WinCrossStep4, Cross, "23">;
type WinCrossOutcome = Expect<Equal<
   WinCrossFinal["winner"], Cross
>>;

// ##################################
// #              TEST              #
// ##################################
// #     GAME That Circle Wins      #
// ##################################
type WinCircleStep1 = Move<InitialRound, Cross, "13">;
type WinCircleStep2 = Move<WinCircleStep1, Circle, "11">;
type WinCircleStep3 = Move<WinCircleStep2, Cross, "33">;
type WinCircleStep4 = Move<WinCircleStep3, Circle, "21">;
type WinCircleStep5 = Move<WinCircleStep4, Cross, "22">;
type WinCircleFinal = Move<WinCircleStep5, Circle, "31">;
type WinCircleOutcome = Expect<Equal<
   WinCircleFinal["winner"], Circle
>>;

// ##################################
// ##################################
// ##################################
// #           Play a Game          #
// ##################################
// ##################################
// ##################################
// 
// This is my attempt to a decent UX (hehe).
// 1. Cross will be the first to move
// 2. Just enter coordinates.
// 
// The status checks below will turn RED when 
// one of the players win or it's a draw
//
// Illegal moves will be compile time errors.
//
type GameStatus =  CrashOrPass<SetError<
  GameLoop<[
   // Cross Win
   "11", "32", "22", "12", 

  // Draw 
  // "13", "23", "33", "12", "22", "11", "32", "31", "21"
  ]
>>>

//@ts-expect-error
type C0 = GameIsOnGoing<GameStatus>

//@ts-expect-error
type C1 = TheGameIsADraw<GameStatus>

//@ts-expect-error
type C2 = CrossHasWon<GameStatus>

//@ts-expect-error
type C3 = CirlceHasWon<GameStatus>


type TheGameIsADraw<T extends Draw<any>> = T
type CrossHasWon<T extends Winner<Cross, any, any>>= T
type CircleHasWon<T extends Winner<Circle, any, any>> = T
type GameIsOnGoing<T extends Round<any, any, any>> = T

type GameLoop<A extends Array<Coordinates>, R extends Round<any,any,any> = InitialRound, P extends Player = Cross> = 
 A extends [infer Head, ...infer Tail] ?
  Tail extends Array<Coordinates> ?
  Head extends AvailableSquares<R["squares"]> ?
    Move<R,P,Head> extends Round<any,any,any> ?
      GameLoop<Tail, Move<R,P,Head>, GetNextPlayer<P>> : 
      Tail extends [] ? Move<R,P,Head> : "__ERROR__: No more moves allowed" 
                                       : `__ERROR__: Square "${Head extends string ? Head : never}" already taken` 
                                       : "__ERROR__: Tail of Coordinate array is malformatted" 
                                       : R

type SetError<T> = [T, T extends `__ERROR__: ${string}` ? true : false]
type CrashOrPass<T extends [unknown, false]> = T[0];



// ##################################
// ##################################
// ##################################
// #         End of program         #
// #       Thanks for reading       #
// ##################################
// ##################################
// ##################################

//type Move<
//  CurrentRound extends Round<Board, P, any>,
//  P extends Player,
//  Position extends AvailableSquares<CurrentRound["squares"]>
//> = HasWon<
//  P,
//  Round<SetSquare<CurrentRound["squares"], Position, P>, GetNextPlayer<P>, CurrentRound>
//> extends true
//  ? Winner<P, CurrentRound, SetSquare<CurrentRound["squares"], Position, P>>
//  : NoMoreSquares<SetSquare<CurrentRound["squares"], Position, P>> extends true
//    ? Draw<CurrentRound>
//    : Round<
//        SetSquare<CurrentRound["squares"], Position, P>,
//        GetNextPlayer<P>,
//        CurrentRound
//      >;
//