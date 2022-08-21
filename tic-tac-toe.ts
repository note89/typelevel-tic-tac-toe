/*/
################
####  Utils ####
################

    We need some typelevel utility functions to help us with the game.
    Equal and Expect functions have been taken from the excellent type-challenges repo.
    https://github.com/type-challenges/type-challenges/blob/master/utils/index.d.ts
*/


/*
 Equal<X,Y> 
 Check if two types are equal.

 ------------------------------------------------------------
 For full discussion around this type, see:
 https://github.com/Microsoft/TypeScript/issues/27024#issuecomment-421529650

> ...It relies on conditional types being deferred when T is not known. 
> Assignability of deferred conditional types relies on an internal isTypeIdenticalTo check, 
> which is only true for two conditional types if:
> 
>   * Both conditional types have the same constraint
>   * The true and false branches of both conditions are the same type
- https://github.com/Microsoft/TypeScript/issues/27024#issuecomment-510924206

--------------------------------------------------------------
*/

type Equal<X, Y> = 
  (<T>() => T extends X ? 1 : 2) extends 
  (<T>() => T extends Y ? 1 : 2) ? true : false;

// Expect<T>
// Give type-level error if T is not true
type Expect<T extends true> = T;



// CartesianProduct<X,Y>
// Example: 
// CartesianProduct<"a" | "b", "c" | "d"> = ["a,c", "a,d", "b,c", "b,d"]
type CartesianProductString<T extends ToStringableTypes,T2 extends ToStringableTypes > = `${T}${T2}`;
type CartesianProductString2<T extends Array<ToStringableTypes>,T2 extends Array<ToStringableTypes>> = `${T[number]}${T2[number]}`;
//type CartesianProductString<T extends ToStringableTypes,T2 extends ToStringableTypes > = `${T},${T2}`;
type ToStringableTypes = string | number | boolean | bigint;

// UnionToIntersection<X>
// Takes a union like `A | B | C` and returns an intersection like `A & B & C`
type UnionToIntersection<U> = 
  (U extends any ? (k: U)=>void : never) extends ((k: infer I)=>void) ? I : never


// Defined between 1 and 1000
type MinusOne<T extends number, arr extends any[] = []> = [
  ...arr,
  ''
]['length'] extends T
  ? arr['length']
  : MinusOne<T, [...arr, '']>

// Defined between 1 and 1000
type PlusOne<T extends number, arr extends any[] = []> = 
  [...arr, '']['length'] extends T
    ? [...arr, '', '']['length']
    : PlusOne<T, [...arr, '']>

/********/

/*
#####################
####  Data Types ####
#####################
*/

interface Circle { __type: "O";     }
interface Cross  { __type: "X";     }
interface Empty  { __type: "Empty"; }
interface Nil    { __type: "Nil";   }

type Player = Cross | Circle;
type Square = Player | Empty;



type FromToInc<From extends number, To extends number, acc extends any[] = []> = From extends PlusOne<To> ? acc : FromToInc<PlusOne<From>, To, [...acc, From]>;
type FromToDec<From extends number, To extends number, acc extends any[] = []> = From extends MinusOne<To> ? acc : FromToDec<MinusOne<From>, To, [...acc, From]>;
type Repeat<T extends any, N extends number, start extends number = 1, acc extends any[] = []> = start extends PlusOne<N> ? acc : Repeat<T, N, PlusOne<start>, [...acc, T]>;
type ToUnion<T extends any[]> = T[number]

///##############################################
// #### SELECT THE SIZE OF THE GAME YOU WANT ####
///##############################################
type Size = 3
///#############################################

type Constructor<Size extends number> = {
  "rows": ToUnion<FromToInc<1,Size>>,
  "columns": ToUnion<FromToInc<1,Size>>,
  "positions": CartesianProductString2<FromToInc<1,Size>,FromToInc<1,Size>>,
}

type Gamey = Constructor<Size>;

type Column = Gamey["columns"];
type Row    = Gamey["rows"]
type Position = Gamey["positions"];

type Zip<T extends any[], U extends any[], Acc extends any[] = []> = 
  T extends [infer Head, ...infer Tail] ? 
  U extends [infer Head2, ...infer Tail2] ? 
  Zip<Tail, Tail2, [...Acc, [Head, Head2]]> : Acc  : Acc;


type Concat<T extends [number, number][]> = {[Key in keyof T]: `${T[Key][0]}${T[Key][1]}`};
type Diagonals<Size extends number> =  Zip<FromToInc<1, Size>, FromToInc<1,Size>> | Zip<FromToInc<1, Size>, FromToDec<Size, 1>>
type Rows<RowNum extends number> =  RowNum extends number ? Zip<Repeat<RowNum,Size>,FromToInc<1,Size>> : never


type Squares = { [s in Position]: Square };


type GetRowPositions<R extends Row> = R extends Row ? [CartesianProductString<R,Column>] : never;
type GetColumnPositions<C extends Column> = C extends Column ? [CartesianProductString<Row,C>] : never;


type A10 = GetRowPositions<Row>

type WinningPositions =
  // Rows
  | GetRowPositions<Row>
  // Columns
  | GetColumnPositions<Column>
  // Diagonals
  | Concat<Diagonals<Size>>

type LookupPositions<Positions extends Array<Position>, S extends Squares> =
  { [Key in keyof Positions ]: S[Positions[Key]] }

type UniqueInSequence<P extends Array<unknown>> =
  P extends Array<unknown> ? UnionToIntersection<P[number]> : never

type GetWinnerOrNever<S extends Squares> =
   UniqueInSequence<LookupPositions<WinningPositions,S>>


type TESTY = {
  TopLeft: Circle;
  TopMiddle: Circle;
  TopRight: Circle;
  MiddleLeft: Circle;
  MiddleMiddle: Cross;
  MiddleRight: Circle;
  BottomLeft: Cross;
  BottomMiddle: Cross;
  BottomRight: Cross;
}


interface HasPrevious<P> {
  previous: P;
}

interface Board<
  S extends Squares,
  P extends Player,
  B extends Board<any, any, any> | Nil
> extends HasPrevious<B> {
  squares: S;
  nextToMove: P;
}

interface Draw<B extends Board<any, any, any>> extends HasPrevious<B> {}

type InitialSquares = { [key in keyof Squares]: Empty };

type InitialBoard = Board<InitialSquares, Cross, Nil>;


type AvailableSquares<B extends Squares> = {
  [key in keyof B]: B[key] extends Empty ? key : never;
}[keyof B];

interface Winner<
  S extends Player,
  PrevB extends Board<any, any, any>,
  Curr extends Squares
> extends HasPrevious<PrevB> {
  winningPosition: Curr;
  winner: S;
}

/* Move */
type Move<
  CurrentBoard extends Board<Squares, P, any>,
  P extends Player,
  Coord extends AvailableSquares<CurrentBoard["squares"]>
> = HasWon<
  P,
  Board<SetSquare<CurrentBoard["squares"], Coord, P>, FlipUser<P>, CurrentBoard>
> extends true
  ? Winner<P, CurrentBoard, SetSquare<CurrentBoard["squares"], Coord, P>>
  : NoMoreSquares<SetSquare<CurrentBoard["squares"], Coord, P>> extends true
  ? Draw<CurrentBoard>
  : Board<
      SetSquare<CurrentBoard["squares"], Coord, P>,
      FlipUser<P>,
      CurrentBoard
    >;


// Utils
type FlipUser<P extends Player> = P extends Cross ? Circle : Cross;

type SetSquare<Sqs extends Squares, K, P> = {
  [key in keyof Sqs]: key extends K ? P : Sqs[key];
};

type NoMoreSquares<S extends Squares> = AvailableSquares<S> extends never
  ? true
  : false;

type HasWon<
  P extends Player,
  B extends Board<any, any, any>
> = P extends GetWinnerOrNever<B["squares"]> ? true : false;
//Equal<P, FindWinnerOrNever<B["squares"]>> 
/* Move end*/

/* WhoWonOrDraw */
type WhoWonOrDraw<A extends Draw<any> | Winner<Player, any, any>> = (
  state: A
) => A extends Winner<infer P, any, any>
  ? PlayerWinnerString<P>
  : "The game was a draw";

// Utils
type PlayerWinnerString<P extends Player> = P extends Circle
  ? "Circle Won the game"
  : "Cross Won the game";
/* WhoWonOrDraw End */

/* TakeMoveBack */
type TakeMoveBack<B extends HasPrevious<Board<any, any, any>>> = B["previous"];
/* TakeMoveBack End */

/* IsPositionOccupied */
type IsPositionOccupied<
  S extends Winner<any, any, any> | Board<any, any, any>,
  C extends Position
> = (
  S extends Winner<any, any, any>
    ? S["winningPosition"][C]
    : S extends Board<any, any, any>
    ? S["squares"][C]
    : never
) extends Empty
  ? false
  : true;

/* IsPositionOccupied End*/

// Test CASES //

type CircleWonString = ReturnType<WhoWonOrDraw<WinCircleFinal>>;
type CrossWonString = ReturnType<WhoWonOrDraw<WinCrossFinal>>;
type DrawString = ReturnType<WhoWonOrDraw<DrawFinal>>;

// No double moves by same player
// prettier-ignore
// @ts-expect-error
type NoDoubleMove = Move<Move<InitialBoard, Cross, "33">, Cross, "13" >;

// No start with Circle
// @ts-expect-error
type NoStartWithCircle = Move<InitialBoard, Circle, "33">;

// No taking a used square
// prettier-ignore
// @ts-expect-error
type NoTakingAUsedSquare = Move<Move<InitialBoard, Cross, "33">,Circle, "33">

// Don't take to many moves back
// @ts-expect-error
type NotToManyMovesBack = TakeMoveBack<InitialBoard>;

type MoveOne = Move<InitialBoard, Cross, "33">;
type BackToNormal = TakeMoveBack<MoveOne>;
type Outcome = Expect<Equal<InitialBoard, BackToNormal>>;

type BackFromDraw = Expect<Equal<TakeMoveBack<DrawFinal>, DrawStep8>>;
type BackFromWin = Expect<Equal<TakeMoveBack<WinCrossFinal>, WinCrossStep4>>;

// IsPositionOccupied

type Test = Expect<
  Equal<IsPositionOccupied<WinCrossFinal, "12">, false>
>;
type Test2 = Expect<Equal<IsPositionOccupied<WinCrossFinal, "33">, true>>;
type Test3 = Expect<Equal<IsPositionOccupied<InitialBoard, "33">, false>>;
// @ts-expect-error
type Test4 = Expect<Equal<IsPositionOccupied<InitialBoard, "33">, true>>;

// DRAW GAME

type DrawStep1 = Move<InitialBoard, Cross, "13">;
type DrawStep2 = Move<DrawStep1, Circle, "23">;
type DrawStep3 = Move<DrawStep2, Cross, "33">;
type DrawStep4 = Move<DrawStep3, Circle, "12">;
type DrawStep5 = Move<DrawStep4, Cross, "22">;
type DrawStep6 = Move<DrawStep5, Circle, "11">;
type DrawStep7 = Move<DrawStep6, Cross, "32">;
type DrawStep8 = Move<DrawStep7, Circle, "31">;
type DrawFinal = Move<DrawStep8, Cross, "21">;

type DrawOutcome = Expect<Draw<any> extends DrawFinal ? true : false>;

// Cross Win
type WinCrossStep1 = Move<InitialBoard, Cross, "13">;
type WinCrossStep2 = Move<WinCrossStep1, Circle, "11">;
type WinCrossStep3 = Move<WinCrossStep2, Cross, "33">;
type WinCrossStep4 = Move<WinCrossStep3, Circle, "21">;
type WinCrossFinal = Move<WinCrossStep4, Cross, "23">;

type WinCrossOutcome = Expect<Equal<
   WinCrossFinal["winner"], Cross
>>;

// Circle Win
type WinCircleStep1 = Move<InitialBoard, Cross, "13">;
type WinCircleStep2 = Move<WinCircleStep1, Circle, "11">;
type WinCircleStep3 = Move<WinCircleStep2, Cross, "33">;
type WinCircleStep4 = Move<WinCircleStep3, Circle, "21">;
type WinCircleStep5 = Move<WinCircleStep4, Cross, "22">;
type WinCircleFinal = Move<WinCircleStep5, Circle, "31">;

type WinCircleOutcome = Expect<Equal<
   WinCircleFinal["winner"], Circle
>>;
