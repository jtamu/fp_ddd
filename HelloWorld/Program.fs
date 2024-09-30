// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

let add1 x = x + 1
let add x y = x + y

let x = add1 5
printfn "5+1= %d" x
let y = add 3 4
printfn "3+4= %d" y

type AppleVariety =
    | GoldenDelicious
    | GrannySmith
    | Fuji

type BananaVariety =
    | Cavendish
    | GrosMichel
    | Manzano

type CherryVariety =
    | Montmorency
    | Bing

type FruitSalad = {
    Apple: AppleVariety
    Banana: BananaVariety
    Cherries: CherryVariety
}

type FruitSnack =
    | Apple of AppleVariety
    | Banana of BananaVariety
    | Cherry of CherryVariety

type ProductCode = ProductCode of string

type Person = { First: string; Last: string }

let aPerson = { First = "John"; Last = "Doe" }
let { First = firstName; Last = lastName } = aPerson
printfn "%s %s" firstName lastName

type OrderQuantity =
    | UnitQuantity of int
    | KilogramQuantity of float

let anOrderQtyInUnits = UnitQuantity 10
let anOrderQtyInKg = KilogramQuantity 2.5

let printQuantity quantity =
    match quantity with
    | UnitQuantity qty -> printfn "%d units" qty
    | KilogramQuantity qty -> printfn "%.1f kg" qty

printQuantity anOrderQtyInUnits
printQuantity anOrderQtyInKg

type CheckNumber = CheckNumber of int

type CardNumber = CardNumber of string

type CardType = Visa | MasterCard

type CreditCardInfo = {
    CardType: CardType
    CardNumber: CardNumber
}

type PaymentMethod =
    | Cash
    | Check of CheckNumber
    | Card of CreditCardInfo

type PaymentAmount = PaymentAmount of decimal
type Currency = EUR | USD

type Payment = {
    Amount: PaymentAmount
    Currency: Currency
    Method: PaymentMethod
}

type PersonalName = {
    FirstName: string
    MiddleInitial: string option
    LastName: string
}

let aList = [1; 2; 3]
let aNewList = 0 :: aList

let printList1 list =
    match list with
    | [] -> printfn "The list is empty"
    | [x] -> printfn "The list has one element: %A" x
    | [x; y] -> printfn "The list has two elements: %A and %A" x y
    | longerList -> printfn "The list has more than two elements: %A" longerList

let printList2 list =
    match list with
    | [] -> printfn "The list is empty"
    | first::rest -> printfn "The list starts with %A and continues with %A" first rest

printList1 aNewList
printList2 aNewList
