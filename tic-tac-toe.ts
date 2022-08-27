//
// To play along
// here is a Typescript playground of the code.
// https://tinyurl.com/typelevel-tic-tac-toe
//
// ##################################
// ##################################
// #          Play a Game           #
// #              of                #
// #--------------------------------#
// #           Typelevel            #
// #          Tic-Tac-Toe           #
// #--------------------------------#
// #              UI                #
// #          STARTS HERE           #
// ##################################
// ##################################
// 
// This is my attempt of a decent UX (hehe).
// 0. Select game board size (3x3, 4x4, 5x5, etc) 
// 1. Cross will be the first to move
// 2. Just enter coordinates.
// 
// The status checks below will turn RED 
// To show what state the game is in.
//
// Illegal moves will be compile-time errors
// with decent error messages.
//
type GameSize = 3;

//   ##############################
type ____________DISPLAY___________ = d
//   ##############################

type GameStatus =  CrashOrPass<SetError<
  GameLoop<[
  //  Ongoing game
  // "13","23","33"

  //  Play a taken position
  //  "11", "11"

  //  Malformed move
  //  "11", "Hello"

  //  Play moves after game is won
  //  "11", "32", "22", "12", "33" "13"

  //  Cross Win
  //  "11", "32", "22", "12", "33" 

  //  Circle Win
  //  "13", "11", "23", "22", "12", "33"

  // Draw 
  // "13", "23", "33", "12", "22", "11", "32", "31", "21"
  ]
>>>


/**********@ts-expect-error*******/
//   ##############################
type _______GAME_YET_TO_START______ = GameYetToStart<GameStatus>
//   ##############################

/**********@ts-expect-error*******/
//   ##############################
type ________GAME_IS_ONGOING_______ = GameIsOngoing<GameStatus>
//   ##############################

/**********@ts-expect-error*******/
//   ##############################
type ________GAME_IS_A_DRAW________ = TheGameIsADraw<GameStatus>
//   ##############################

/**********@ts-expect-error*******/
//   ##############################
type ________CROSS_HAS_WON_________ = CrossHasWon<GameStatus>
//   ##############################

/**********@ts-expect-error*******/
//   ##############################
type _______CIRCLE_HAS_WON_________ = CircleHasWon<GameStatus>
//   ##############################

// ##################################
// ##################################
// #--------------------------------#
// #           Typelevel            #
// #          Tic-Tac-Toe           #
// #--------------------------------#
// #              UI                #
// #           ENDS HERE            #
// #--------------------------------#
// #                                #
// #        THE UI CODE IS AT       #
// #       THE END OF THE FILE      #
// #                                #
// #--------------------------------#
// ##################################
// ##################################


// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

// #############################
// #############################
// ####                     ####
// ####     Requirements    ####
// ####                     ####
// #############################
// #############################
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

// ################################
// ################################
// ####          CODE          ####
// ####         STARTS         ####
// ####          HERE          ####
// ################################
// ################################

// #############################
// #############################
// ####                     ####
// ####        UTILS        ####
// ####                     ####
// #############################
// #############################
//
// TIP: Consider skipping this section and going back to reading the code
// when you run into one of the functions mention here

// CartesianProduct<X,Y>
// Example: 
// CartesianProduct<"a" | "b", "c" | "d"> = "ac" | "ad" | "bc" | "bd"
type CartesianProductString<T1 extends ToStringableTypes,T2 extends ToStringableTypes > = `${T1}${T2}`;
type ToStringableTypes = string | number | boolean | bigint;
// -----------
// Explanation: CartesianProduct<X,Y>
// -----------
// [The Algebra of algebraic data types](https://gist.github.com/gregberns/5e9da0c95a9a8d2b6338afe69310b945)
// Is a great resource if you want to learn more about this
//
// `${T1}${T2}`
// T1 and T2 can both be a union of different types.
// Say
// `${ "a" | "b" | "c" }${ "d" | "e" | "f" }` 
// We have three choices for the first part of the string.
// So a union of three members, with the first value fixed in each string is equivalent to the first type
//  `a${"d"| "e" | "f"} | `b${"d" | "e" | "f"}` | `c{"d" | "e" | "f"}`
// We now have three choices for the second part of the string
// If we multiply them out, we get the cartesian product of the two initial types.
// "ad"| "ae" | "af" | "bd" | "be" | "bf" | "cd" | "ce" | "cf"

// After reading the algebra of algebraic data types, you can see that
// we need a member from both T1 and T2, so we have T1 * T2 = choices to create our string
// cartesian product of "a" | "b" | "c" and "d" | "e" | "f"  is 3 * 3 = 9 strings

//  1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 = 9
//  "ad"| "ae" | "af" | "bd" | "be" | "bf" | "cd" | "ce" | "cf"

//  (1 * (1 + 1 + 1)) + (1 * (1 + 1 + 1)) + (1 * (1 + 1 + 1)) = 9
//  `a${"d"| "e" | "f"} | `b${"d" | "e" | "f"}` | `c${"d" | "e" | "f"}`

//  (1 + 1 + 1) * (1 + 1 + 1) = 9
// `${ "a" | "b" | "c" }${ "d" | "e" | "f" }` 
//  (3) * (3) = 9


// UnionToIntersection<X>
// Takes a union like `A | B | C` and returns an intersection like `A & B & C`
type UnionToIntersection<U> = 
     _PutUnionMembersIntoFunctionArgumentPosition<U> extends ((k: infer I)=>void) ? I : never

type _PutUnionMembersIntoFunctionArgumentPosition<U> = U extends any ? (k: U)=>void  : never;
// -----------
// Explanation: UnionToIntersection<U> 
// Creator: Jcalz 
// https://github.com/microsoft/TypeScript/issues/29594#issuecomment-507673155
// -----------
// This one is worthy quite a bit of explanation. It has been somewhat of a debated topic,
// if it's a hack or not, and if it should be included in official lib.d.ts or not.
// I'm most certainly don't think this is a hack at all, if this would break in a future version of 
// TypeScript, i would say that would break a lot of other things.
//
// Sources:
// https://stackoverflow.com/questions/50374908/transform-union-type-to-intersection-type/50375286#50375286
// https://github.com/microsoft/TypeScript/issues/29594#issuecomment-507673155
//
//
// ##############
// ### PART 1 ###
// ##############
// PutUnionMembersIntoFunctionArgumentPosition<U> = U extends any ? (k: U)=>void : never
// ##############
//
// This is using U as a 'naked' type parameter.
// Seems this lingo is no longer part of the official docs, which is a shame,
// will make it harder to understand, why things act like they do.
// https://www.typescriptlang.org/docs/handbook/2/conditional-types.html#distributive-conditional-types
// 
// Naked type parameters distributes over a union. 
// this means the conditional part will be applied to each member of the union.
// and the result will be a union of all applications of the conditional.
// https://stackoverflow.com/questions/51651499/typescript-what-is-a-naked-type-parameter
//
// Let me show you. 
//
// PutUnionMembersIntoFunctionArgumentPosition<U> = U extends any ? (k: U)=>void : never
// 
// type ABC = "a" | "b" | "c"
// PutUnionMembersIntoFunctionArgumentPosition<ABC>
// 
// Replace U with supplied type ABC, U is naked so do a replacement
// for each member in ABC and make a union of the results.
// 
//   "a" extends any ? (k: "a")  => void : never 
// | "b" extends any ? (k: "b")  => void : never 
// | "c" extends any ? (k: "c")  => void : never
// 
// Which simplifies to 
//   (k: "a")  => void  
// | (k: "b")  => void  
// | (k: "c")  => void
// 
// Many people (including myself in the past) think that a type parameter
// get's inlined into the type level function like.
//
// PutUnionMembersIntoFunctionArgumentPosition<"a" | "b" | "c">
// ("a" | "b" | "c") extends any ? (k: "a" | "b" | "c")  => void : never
//
//
// --- THIS IS WRONG!---
//
//
// To get that behavior, the type parameter need to be 'clothed'.
//
// Clothed example.
// PutInFunctionArgumentPosition<U> = ([U] extends any ? (k: U)=>void : never)
//
// PutInFunctionArgumentPosition<ABC>
//
// Replace U with supplied type ABC, U is clothed so do the replacement
// inline.
// 
// (["a" | "b" | "c"] extends any ? (k: "a" | "b" | "c")=>void : never)
// 
// Which simplifies to
// (k: "a" | "b" | "c") => void
//
// #### QUIZ ####
// To test yourself on the difference between naked and clothed,
// Think of the results of Inline and calling the functions
// with ABC and ABC3
//
// type ABC  = "a" | "b" | "c"
// type ABC3 = ABC | 3
//
// type ABC_Inline_Extends_A      = ABC extends   "a"    ? (k: ABC) => void : never
// type Naked_Extends_A<T>        =  T  extends   "a"    ? (k:  T ) => void : never
// type Naked_Extends_String<T>   =  T  extends  string  ? (k:  T ) => void : never
// type Clothed_Extends_String<T> = [T] extends [string] ? (k:  T ) => void : never
//
// #### QUIZ Answers ####
// https://tinyurl.com/typescript-quiz
//
// _Note:_
// _If we would just change the type of PutUnionMembersIntoFunctionArgumentPosition to be
// (k: U) => void, we would have also gotten (k: "a" | "b" | "c") => void.
// We need the U extends any ? to distribute the union._
//
// Moving on.
// 
// ##############
// ### PART 2 ###
// ##############
// ...PART1 extends ((k: infer I)=>void) ? I : never
// ##############
// 
// So Part 1 gave use the result
//   (k: "a")  => void  
// | (k: "b")  => void  
// | (k: "c")  => void
//
// There is no type param that we are replacing here, so 
// we can just inline this result. 
// 
// (    (k: "a")  => void) 
//    | (k: "b")  => void) 
//    | (k: "c")  => void)
// ) extends ((k: infer I)=>void) ? I : never
//
// (If you have not heard about `infer` before look at these docs.
// https://learntypescript.dev/09/l2-conditional-infer)
//
// We are saying here that there is a function  ((k: infer I) => void) that can be 
// either      (k: "a") => void 
//          or (k: "b") => void 
//          or (k: "c") => void
// It's not the argument to the function that can be "a" | "b" | "c".
// There are three separate functions with those argument, and we DON'T know which one we will have.
// Therefore the only SAFE argument to ((k: infer I) => void) the intersection of all possible arguments.
// 
// That might had been a bit confusing, so let's look at one example
//
// type T = (k: HasName) => void | (k: HasAge) => void
// const func : T = ...
// 
// If we now want to call func what argument do we need to give it ? 
// Well we don't know if T is (k: HasName) => void or (k: HasAge) => void.
// So we need to call func with an argument that is both. 
// type Dog = HasName & HasAge
// const dog : Dog = createDog("Fido", 3)
// func(dog)
// 
// Thank AnyHowStep for this answer that helped me write this section:
// https://github.com/microsoft/TypeScript/issues/29594#issuecomment-507701193
//

// Example: ToUnion<[1,2,3]> = 1 | 2 | 3
//
// Explanation: ToUnion<T>
// Array has a index signature of `number`
// type Array<T> = { [index: number]: T ....}
// so by indexing with 'number' we get back T.
// https://www.typescriptlang.org/docs/handbook/2/objects.html#index-signatures
type ToUnion<T extends Array<any>> = T[number]


// Zip<A,B>
// Zip two arrays together
// Example: Zip<[1,2,3],["a","b","c"]> = [[1,"a"],[2,"b"],[3,"c"]]
type Zip<T extends any[], U extends any[], Acc extends any[] = []> = 
  T extends [infer Head, ...infer Tail] ? 
  U extends [infer Head2, ...infer Tail2] ? 
  Zip<Tail, Tail2, [...Acc, [Head, Head2]]> : Acc  : Acc;


// StringConcatTuples<T>
// Join tuples together to strings
// Example: StringConcatTuples<[[1,2],[3,4]]> = ["12","34"]
//
// Explanation: StringConcatTuples<T>
// Here we are using a Mapped Types.
// https://www.typescriptlang.org/docs/handbook/2/mapped-types.html
// It' allows you to loop over the keys of a type.
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
// Defined between 0 and 999
// Start with an array of length 0 and check if it is equal to N
// If it is add one element to the array and return it's length.
// If not recursively call PlusOne with an array that is one element longer.
// This way we get N + 1
type PlusOne<N extends number, Arr extends any[] = []> = 
  [...Arr]['length'] extends N
    ? [...Arr, unknown]['length']
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
// #####################
// ####  Data Types ####
// #####################
// #####################

interface Circle { __type: "O";     }
interface Cross  { __type: "X";     }
interface Empty  { __type: "Empty"; }
interface Nil    { __type: "Nil";   }

type Player = Cross | Circle;
type Square = Player | Empty;


// ##############################################
type Size = GameSize;
// ##############################################
// (Size was here before i made the UI at the top therefore the redeclaration)
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
// 1. Create a union of all the winning positions
//    ["11", "12", "13"] | ["21", "22", "23"] | ... 
//
// 2. Check what the current state of all of them are
//    [Circle, Cross, Empty] | [Circle, Circle, Circle] ... 
//
// 3. Create an intersection those states, and remove the array/tuple
//    [Circle & Cross & Empty] | [Circle & Circle & Circle] | ... 
//    [never] | [Circle]
//    never | Circle
//    Circle
//
// Only time we get a value is when all three are the same
//
// Now we can check if the player is the winner, 
// by checking if the intersection contains a player.
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
// ["11", "12", "13"] | ["21", "22", "23"] | ...
// All the winning positions for a Tic-Tac-Toe game.
type WinningPositions =
  // Rows
  | GetRowPositions<Column, Row>
  // Columns
  | GetColumnPositions<Column, Row>
  // Diagonals
  | GetDiagonalPositions<Size>

// ###################################
// ####           Board           ####
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
type LookupCoordinates<Coords extends Array<Coordinates>, B extends Board> =
  { [Key in keyof Coords ]: B[Coords[Key]] }

// If there is any Array that contains only the same element
// then that element will be returned
type UniqueInSequence<P extends Array<unknown>> =
  P extends Array<unknown> ? UnionToIntersection<P[number]> : never

// If there is a winner in the Squares provided then the winner is returned.
type GetWinner<B extends Board> =
   UniqueInSequence<LookupCoordinates<WinningPositions,B>>


// ###################################
// ####        Game States        ####
// ###################################
// The game can be in one of three states:
// 1. Round in progress
// 2. Won
// 3. Draw

type GameStates = Round<any, any, any> | Draw<any, any> | Winner<any, any, any>

// ###################################
// ####           Round           ####
// ###################################
// 
// A Round has a bunch of Squares and a Player that is next to move.
// It also has a previous Round or Nil, to be able to allow for undoing moves.
interface Round<
  B extends Board,
  P extends Player,
  R extends Round<any, any, any> | Nil
> extends HasPrevious<R> {
  __tag: "round";
  board: B;
  nextToMove: P;
}

// We separate our HasPrevious interface from the Round interface
// To be able to use it in other interfaces.
// And have type level functions where the only constraint is that
// it has the HasPrevious interface.
interface HasPrevious<R> {
  previous: R;
}

// ###################################
// ####   Initial board & round   ####
// ###################################
//
type InitialBoard = { [key in keyof Board]: Empty };
type InitialRound = Round<InitialBoard, Cross, Nil>;


interface Winner<
  P extends Player,
  PrevR extends Round<any, any, any>,
  Curr extends Board
> extends HasPrevious<PrevR> {
  __tag: "winner",
  winningPosition: Curr;
  winner: P;
}

interface Draw<R extends Round<any, any, any>, B extends Board> extends HasPrevious<R> {__tag: "draw";}

type GetBoard<R extends GameStates>
                  = 
                  R extends Draw<any, infer B> ? B
                  : R extends Round<infer B, any, any>              ? B
                  : R extends Winner<any, any, infer B>             ? B
                  : never;

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
// Last argument is not allowed to be passed in, but is used 
// to reduce duplication in the function body.
type Move<
  CurrentRound extends Round<Board, P, any>,
  P extends Player,
  Position extends AvailableSquares<CurrentRound["board"]>,
  _NextBoard extends Board = SetSquare<CurrentRound["board"], Position, P>,
  _NextRound extends Round<any,any,any> = Round<_NextBoard, GetNextPlayer<P>, CurrentRound>
> = 
HasWon<P,_NextRound> extends true 
  ? Winner<P, CurrentRound, _NextBoard>
  : NoMoreSquares<_NextBoard> extends true
    ? Draw<CurrentRound, _NextBoard>
    : _NextRound
    

// ###########################################
// ####        Game State Functions       ####
// ###########################################

// Squares that are possible to play on
type AvailableSquares<B extends Board> = {
  [Coordinate in keyof B]: B[Coordinate] extends Empty ? Coordinate : never;
}[keyof B];

type GetNextPlayer<P extends Player> = P extends Cross ? Circle : Cross;

// Sets a Square to a Player
type SetSquare<B extends Board, PositionToSet, Player> = {
  [Pos in keyof B]: Pos extends PositionToSet ? Player : B[Pos];
};

// Checks if there is no more squares to play on.
type NoMoreSquares<B extends Board> = AvailableSquares<B> extends never
  ? true
  : false;

// Checks if a player has won.
type HasWon<
  P extends Player,
  B extends Round<any, any, any>
> = P extends GetWinner<B["board"]> ? true : false;


// ###########################################
// ####        Extra Functions from       ####
// ####             Requirements          ####
// ###########################################
type DrawString = "The game was a draw";
type WhoWonOrDraw<A extends Draw<any,any> | Winner<Player, any, any>> = (
  state: A
) => A extends Winner<infer P, any, any>
  ? PlayerWinnerString<P>
  : DrawString;

// Util
type CircleWonString = "Circle Won the game"
type CrossWonString = "Cross Won the game"
type PlayerWinnerString<P extends Player> = P extends Circle
  ? CircleWonString
  : CrossWonString

type TakeMoveBack<B extends HasPrevious<Round<any, any, any>>> = B["previous"];

//  
// We check if there if the Square is Empty in the Position provided. 
// Depending on the game state we look at different fields. 
// We return a boolean to indicate if the square is occupied or not.
// We don't allow the function to be called with Draw, because a Draw game
// has by definition no squares to play on.
// Which is a bit stronger then the official requirement, and we could 
// just return true if the game is Draw.
type IsPositionOccupied<
  RW extends Winner<any, any, any> | Round<any, any, any>,
  Coord extends Coordinates,
  B extends Board = GetBoard<RW>
> = (
  B[Coord] 
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

// ##################################
// #              TEST              #
// ##################################
// #         Correct strings        #
// ##################################
type CircleWonStringTest = Expect<Equal<ReturnType<WhoWonOrDraw<WinCircleFinal>>,  CircleWonString>>;
type CrossWonStringTest  = Expect<Equal<ReturnType<WhoWonOrDraw<WinCrossFinal>>,   CrossWonString>>;
type DrawStringTest      = Expect<Equal<ReturnType<WhoWonOrDraw<DrawFinal>>,       DrawString>>;

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
type DrawOutcome = Expect<Draw<any,any> extends DrawFinal ? true : false>;

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


// ################################
// ################################
// ####        UI CODE         ####
// ################################
// ################################

// ################################
// ####       UI Helpers       ####
// ################################

// 'd' is just to hide the variable as much as possible in the UI
type d = PrintGameDisplay<GameStatus>

// These are functions give type errors when game is not in their state
// Then we combine that with @ts-expect-error to flip that behavior
type TheGameIsADraw<T extends Draw<any, any>> = T
type CrossHasWon<T extends Winner<Cross, any, any>>= T
type CircleHasWon<T extends Winner<Circle, any, any>> = T
type GameIsOngoing<T extends Round<any, any, Round<any,any,any>>> = T
type GameYetToStart<T extends InitialRound> = T

// ################################
// ####       GAME LOOP        ####
// ################################

// TODO, allow number input
// Give error is coordinate is not valid
type GameLoop<A extends Array<Coordinates>, 
              R extends Round<any,any,any> = InitialRound, 
              P extends Player = Cross> = 
 A extends [infer Head, ...infer Tail] ?
   Tail extends Array<Coordinates> ?
   Head extends AvailableSquares<R["board"]> ?
     Move<R,P,Head> extends Round<any,any,any> ?
       GameLoop<Tail, Move<R,P,Head>, GetNextPlayer<P>> : 
       Tail extends [] ? Move<R,P,Head> 
         : `${ERROR_ID} No more moves allowed, game is over`
         : `${ERROR_ID} Square '${Head extends string ? Head : never}' already taken` 
         : `${ERROR_ID} Tail of Coordinate array is malformatted`
         : R

type ERROR_ID = "__ERROR__:"
type SetError<T> = [T, T extends `${ERROR_ID}${string}` ? "error" : "noError"]
type CrashOrPass<T extends [unknown, "noError"]> = T[0];
type ToNumber<T extends `${number}`> = T extends `${infer N}` ? N : never;
type A = ToNumber<Coordinates>


// ################################
// ####         DISPLAY        ####
// ################################
//
// We want to produce a UI like 
// {
//   3: [X,X,O],
//   2: [X,_,O],
//   1: [_,_,O],
// }
//
// To show the state of the game when hovering the type
//

type PrintGameDisplay<T extends GameStates> = ShowBoard<GetBoard<T>>

// Some shorter and nicer looking symbols for the Display
interface X {}
interface O {}
interface _ {}

// UIBoard
// Coordinates to something that we want to Display
type UIBoard = {[s in Coordinates]: unknown}

/// #### DISPLAY HELPERS ####

type ShowBoard<B extends Board> = ShowUIBoard<ToUIBoard<B>>;

// ToUIBoard
// The particular board we want to show
// Structurally, a UIBoard
type ToUIBoard<B extends Board> = {[s in Coordinates]: ChangeSquare<B[s]>};

type ChangeSquare<S extends Square> = S extends Cross  ? X
                                    : S extends Circle ? O
                                    : S extends Empty  ? _
                                    : never;

// We want our coordinate system to have 1,1 bottom left
// and Size,Size top right
type RowsBackwards  = FromToDec<Size,1>
type ColumnsForward = FromToInc<1,Size>

// ShowUIBoard
type ShowUIBoard<B extends UIBoard > = 
  {[key in keyof RowsBackwards as KeyToValue<RowsBackwards,key>]: RowsBackwards[key] extends number 
    ? TupleCoordinateLookups<Zip<ColumnsForward,Repeat<RowsBackwards[key], Size>>, B> : never} 

type KeyToValue<T extends Array<unknown>, K extends keyof T> = 
    K extends `${number}` ? T[K] : never;

type Repeat<T, N extends number, arr extends Array<T> = []> = 
    N extends 0 ? arr : Repeat<T, MinusOne<N>, [T, ...arr]>;

type TupleCoordinateLookups<T extends [number, number][], B extends UIBoard> = 
  {[Key in keyof T]: TupleCoordinateLookup<T[Key], B>};

type TupleCoordinateLookup<T extends [number, number], B extends UIBoard> = 
  `${T[0]}${T[1]}` extends keyof B ? B[`${T[0]}${T[1]}`] : never;



type Test123123 = d
// ##################################
// ##################################
// ##################################
// #         End of program         #
// #       Thanks for reading       #
// ##################################
// ##################################
// ##################################
