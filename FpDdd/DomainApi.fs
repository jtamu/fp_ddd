namespace OrderTaking.Domain

open System

type ResultBuilder() =
    member this.Return(x) = Ok x
    member this.Bind(m, f) = Result.bind f m

module Result =
    type Prepend<'a, 'b> = Result<'a, 'b> -> Result<'a list, 'b list> -> Result<'a list, 'b list>

    let prepend: Prepend<'a, 'b> =
        fun firstR restR ->
            match firstR, restR with
            | Ok first, Ok rest -> Ok(first :: rest)
            | Error e, Ok rest -> Error [ e ]
            | Ok first, Error e -> Error e
            | Error e1, Error e2 -> Error(e1 :: e2)

    let sequence aListOfResults =
        let initialValue = Ok []
        List.foldBack prepend aListOfResults initialValue

// ---------------------------------------
// 入力データ
// ---------------------------------------


type UnvalidatedCustomerInfo =
    { FirstName: string
      LastName: string
      EmailAddress: string }

type UnvalidatedAddress = string

type UnvalidatedOrderLine =
    { OrderLineId: string
      ProductCode: string
      Quantity: float }

type UnvalidatedOrder =
    { OrderId: string
      CustomerInfo: UnvalidatedCustomerInfo
      ShippingAddress: UnvalidatedAddress
      BillingAddress: UnvalidatedAddress
      Lines: UnvalidatedOrderLine list }

// ---------------------------------------
// 入力コマンド
// ---------------------------------------

type DateTime = Undefined

type Command<'data> =
    { Data: 'data
      Timestamp: DateTime
      UserId: string }

type PlaceOrderCommand = Command<UnvalidatedOrder>

// ---------------------------------------
// パブリックAPI
// ---------------------------------------

type AsyncResult<'success, 'failure> = Async<Result<'success, 'failure>>

type CheckedAddress =
    { AddressLine1: string
      AddressLine2: string
      AddressLine3: string
      AddressLine4: string
      City: string
      ZipCode: string }

type AddressValidationError = AddressValidationError of string

type ServiceInfo = { Name: string; Endpoint: string }

type RemoteServiceError =
    { Service: ServiceInfo
      Exception: Exception }

// type CheckAddressExists = UnvalidatedAddress -> AsyncResult<CheckedAddress, AddressValidationError>

type CheckAddressExists = UnvalidatedAddress -> Result<CheckedAddress, RemoteServiceError>

module Domain =
    let result = ResultBuilder()

    let serviceExceptionAdapter serviceInfo serviceFn arg =
        try
            Ok(serviceFn arg)
        with :? TimeoutException as ex ->
            Error
                { Service = serviceInfo
                  Exception = ex }

    type String50 = private String50 of string

    module String50 =
        let create str =
            if String.IsNullOrEmpty(str) then
                failwith "String cannot be null or empty"
            elif str.Length > 50 then
                failwith "String cannot exceed 50 characters"
            else
                String50 str

        let createOption str = String50 str |> Some

    type OrderId = OrderId of String50

    type PersonName =
        { FirstName: String50
          LastName: String50 }

    type EmailAddress = private EmailAddress of string

    module EmailAddress =
        let create str =
            if String.IsNullOrEmpty(str) then
                failwith "Email cannot be null or empty"
            else
                EmailAddress str

    type CustomerInfo =
        { Name: PersonName
          EmailAddress: EmailAddress }

    let toCustomerInfo (customer: UnvalidatedCustomerInfo) : CustomerInfo =
        let firstName = customer.FirstName |> String50.create
        let lastName = customer.LastName |> String50.create
        let emailAddress = customer.EmailAddress |> EmailAddress.create

        let customerInfo: CustomerInfo =
            { Name =
                { FirstName = firstName
                  LastName = lastName }
              EmailAddress = emailAddress }

        customerInfo

    type ZipCode = private ZipCode of string

    module ZipCode =
        let create str =
            if String.IsNullOrEmpty(str) then
                failwith "Zip code cannot be null or empty"
            elif str.Length <> 5 && str.Length <> 9 then
                failwith "Zip code must be either 5 or 9 characters long"
            else
                ZipCode str

    type Address =
        { AddressLine1: String50
          AddressLine2: String50 option
          AddressLine3: String50 option
          AddressLine4: String50 option
          City: String50
          ZipCode: ZipCode }

    let toAddress (checkAddressExists: CheckAddressExists) unvalidatedAddress =
        let checkedAddress = checkAddressExists unvalidatedAddress

        match checkedAddress with
        | Ok okAddress ->
            let addressLine1 = okAddress.AddressLine1 |> String50.create
            let addressLine2 = okAddress.AddressLine2 |> String50.createOption
            let addressLine3 = okAddress.AddressLine3 |> String50.createOption
            let addressLine4 = okAddress.AddressLine4 |> String50.createOption
            let city = okAddress.City |> String50.create
            let zipCode = okAddress.ZipCode |> ZipCode.create

            let address: Address =
                { AddressLine1 = addressLine1
                  AddressLine2 = addressLine2
                  AddressLine3 = addressLine3
                  AddressLine4 = addressLine4
                  City = city
                  ZipCode = zipCode }

            Ok address

        | Error error -> Error error

    type WidgetCode = WidgetCode of string
    type GizmoCode = GizmoCode of string

    type ProductCode =
        private
        | Widget of WidgetCode
        | Gizmo of GizmoCode

    module ProductCode =
        let create str =
            if String.IsNullOrEmpty(str) then
                failwith "Product code cannot be null or empty"
            elif str.StartsWith("W") then
                Widget(WidgetCode str)
            elif str.StartsWith("G") then
                Gizmo(GizmoCode str)
            else
                failwith "Invalid product code format"

    type ValidationError =
        { FieldName: string
          ErrorDescription: string }

    type PricingError = PricingError of string

    type PlaceOrderError =
        | Validation of ValidationError list
        | Pricing of PricingError
        | RemoteService of RemoteServiceError


    // ---------------------------------------
    // 注文のライフサイクル
    // ---------------------------------------

    type OrderLineId = private OrderLineId of string

    module OrderLineId =
        let create str =
            if String.IsNullOrEmpty(str) then
                failwith "OrderLineId must not be null or empty"
            else
                OrderLineId str

    type UnitQuantity = private UnitQuantity of int

    module UnitQuantity =
        let create qty =
            if qty < 1 then
                // Error "Quantity must be at least 1"
                failwith "Quantity must be at least 1"
            else if qty > 1000 then
                // Error "Quantity must be at most 1000"
                failwith "Quantity must be at most 1000"
            else
                // Ok(UnitQuantity qty)
                UnitQuantity qty

        let value (UnitQuantity qty) = qty

    type KilogramQuantity = private KilogramQuantity of float

    module KilogramQuantity =
        let create qty = KilogramQuantity qty

        let value (KilogramQuantity qty) = qty

    type OrderQuantity =
        | Unit of UnitQuantity
        | Kilogram of KilogramQuantity

    module OrderQuantity =
        let value: OrderQuantity -> float =
            fun qty ->
                match qty with
                | Unit uq -> UnitQuantity.value uq |> float
                | Kilogram kq -> KilogramQuantity.value kq

    type ValidatedOrderLine =
        { OrderLineId: OrderLineId
          ProductCode: ProductCode
          Quantity: OrderQuantity }


    // ---------------------------------------
    // 内部ステップの定義
    // ---------------------------------------

    // ----------注文の検証-------------------

    type PredicateToPassThru<'a> = string -> ('a -> bool) -> 'a -> 'a

    let predicateToPassThru: PredicateToPassThru<'a> =
        fun errorMsg f x -> if not (f x) then failwith errorMsg else x

    type CheckProductCodeExists = ProductCode -> bool

    type ToProductCode = CheckProductCodeExists -> string -> ProductCode

    let toProductCode: ToProductCode =
        fun checkProductCodeExists str ->
            ProductCode.create str
            |> predicateToPassThru "Invalid product code" checkProductCodeExists

    type ToOrderQuantity = ProductCode -> float -> OrderQuantity

    let toOrderQuantity: ToOrderQuantity =
        fun productCode qty ->
            match productCode with
            | Widget _ -> qty |> int |> UnitQuantity.create |> Unit
            | Gizmo _ -> qty |> KilogramQuantity.create |> Kilogram

    type ValidationResponse<'a> = AsyncResult<'a, ValidationError list>

    type ToValidatedOrderLine =
        CheckProductCodeExists -> UnvalidatedOrderLine -> Result<ValidatedOrderLine, ValidationError>

    let toValidatedOrderLine: ToValidatedOrderLine =
        fun checkProductCodeExists unvalidatedOrderLine ->
            let orderLineId = unvalidatedOrderLine.OrderLineId |> OrderLineId.create

            let productCode =
                unvalidatedOrderLine.ProductCode |> toProductCode checkProductCodeExists

            let orderQuantity = unvalidatedOrderLine.Quantity |> toOrderQuantity productCode

            let validatedOrderLine: ValidatedOrderLine =
                { OrderLineId = orderLineId
                  ProductCode = productCode
                  Quantity = orderQuantity }

            Ok validatedOrderLine

    type ValidatedOrder =
        { OrderId: OrderId
          CustomerInfo: CustomerInfo
          ShippingAddress: Address
          BillingAddress: Address
          Lines: ValidatedOrderLine list }


    // type ValidateOrder =
    //     CheckProductCodeExists -> CheckAddressExists -> UnvalidatedOrder -> ValidationResponse<ValidatedOrder>

    type ValidateOrder =
        CheckProductCodeExists -> CheckAddressExists -> UnvalidatedOrder -> Result<ValidatedOrder, PlaceOrderError>

    let validateOrder: ValidateOrder =
        fun checkProductCodeExists checkAddressExists unvalidatedOrder ->
            let orderId: OrderId = unvalidatedOrder.OrderId |> String50 |> OrderId
            let customerInfo: CustomerInfo = unvalidatedOrder.CustomerInfo |> toCustomerInfo

            result {
                let! shippingAddress =
                    unvalidatedOrder.ShippingAddress
                    |> toAddress checkAddressExists
                    |> Result.mapError RemoteService

                let! billingAddress =
                    unvalidatedOrder.BillingAddress
                    |> toAddress checkAddressExists
                    |> Result.mapError RemoteService

                let! orderLines =
                    unvalidatedOrder.Lines
                    |> List.map (toValidatedOrderLine checkProductCodeExists)
                    |> Result.sequence
                    |> Result.mapError Validation

                return
                    { OrderId = orderId
                      CustomerInfo = customerInfo
                      ShippingAddress = shippingAddress
                      BillingAddress = billingAddress
                      Lines = orderLines }
            }




    type Price = private Price of int

    module Price =
        let create value = Price value

        let multiply qty (Price p) = qty * p |> create

        let value (Price p) = p

    type BillingAmount = private BillingAmount of Price

    module BillingAmount =
        let sumPrices prices =
            prices |> List.map (fun (Price p) -> p) |> List.sum |> Price |> BillingAmount

        let value (BillingAmount b) = Price.value b

    type PricedOrderLine =
        { OrderLineId: OrderLineId
          ProductCode: ProductCode
          Quantity: OrderQuantity
          LinePrice: Price }

    type PricedOrder =
        { OrderId: OrderId
          CustomerInfo: CustomerInfo
          ShippingAddress: Address
          BillingAddress: Address
          Lines: PricedOrderLine list
          AmountToBill: BillingAmount }

    type OrderPlaced = PricedOrder

    type BillableOrderPlaced =
        { OrderId: OrderId
          BillingAddress: Address
          AmountToBill: BillingAmount }


    // ----------注文の価格計算-------------------

    type GetProductPrice = ProductCode -> Price

    type PriceOrder = GetProductPrice -> ValidatedOrder -> Result<PricedOrder, PricingError>

    type PriceOrderLine = GetProductPrice -> ValidatedOrderLine -> PricedOrderLine

    let priceOrderLine: PriceOrderLine =
        fun getProductPrice validatedOrderLine ->
            let productCode = validatedOrderLine.ProductCode
            let price = getProductPrice productCode
            let quantity = validatedOrderLine.Quantity |> OrderQuantity.value |> int
            let linePrice = Price.multiply quantity price

            let pricedOrderLine: PricedOrderLine =
                { OrderLineId = validatedOrderLine.OrderLineId
                  Quantity = validatedOrderLine.Quantity
                  LinePrice = linePrice
                  ProductCode = productCode }

            pricedOrderLine

    let priceOrder: PriceOrder =
        fun getProductPrice validatedOrder ->
            let orderLines = validatedOrder.Lines |> List.map (priceOrderLine getProductPrice)

            let amountToBill =
                orderLines |> List.map (fun line -> line.LinePrice) |> BillingAmount.sumPrices

            let pricedOrder: PricedOrder =
                { OrderId = validatedOrder.OrderId
                  CustomerInfo = validatedOrder.CustomerInfo
                  Lines = orderLines
                  BillingAddress = validatedOrder.BillingAddress
                  ShippingAddress = validatedOrder.ShippingAddress
                  AmountToBill = amountToBill }

            Ok pricedOrder

    let priceOrderAdapted getProductPrice validatedOrder =
        priceOrder getProductPrice validatedOrder |> Result.mapError Pricing

    type HtmlString = HtmlString of string

    type OrderAcknowledgement =
        { EmailAddress: EmailAddress
          Letter: HtmlString }

    type OrderAcknowledgementSent =
        { OrderId: OrderId
          EmailAddress: EmailAddress }

    type PlaceOrderEvent =
        | OrderPlaced of OrderPlaced
        | BillableOrderPlaced of BillableOrderPlaced
        | AcknowledgementSent of OrderAcknowledgementSent

    type PlaceOrderEvents =
        { Acknowledgement: OrderAcknowledgementSent
          OrderPlaced: OrderPlaced
          BillableOrderPlaced: BillableOrderPlaced }

    type PlaceOrder = UnvalidatedOrder -> PlaceOrderEvents






    type CustomerId = Undefined

    type ShippingAddress = Undefined
    type BillingAddress = Undefined

    type NotEmptyList<'a> = { First: 'a; Rest: 'a list }

    type Order =
        { Id: OrderId
          CustomerId: CustomerId
          ShippingAddress: ShippingAddress
          BillingAddress: BillingAddress
          OrderLines: NotEmptyList<PricedOrderLine>
          AmountToBill: BillingAmount }

    type EnvelopeContents = EnvelopeContents of string

    type QuoteForm = Undefined

    type OrderForm = Undefined

    type CategorizedMail =
        | Quote of QuoteForm
        | Order of OrderForm

    type CategorizeInboundMail = EnvelopeContents -> CategorizedMail

    type ProductCatalog = Undefined

    type CalculatePrices = OrderForm -> ProductCatalog -> PricedOrder


    type CreateOrderAcknowledgmentLetter = PricedOrder -> HtmlString

    type SendResult =
        | Sent
        | NotSent

    // type SendOrderAcknowledgment = OrderAcknowledgement -> Async<SendResult>
    type SendOrderAcknowledgment = OrderAcknowledgement -> SendResult

    // type AcknowledgeOrder =
    //     CreateOrderAcknowledgmentLetter
    //         -> SendOrderAcknowledgment
    //         -> PricedOrder
    //         -> Async<OrderAcknowledgementSent option>
    type AcknowledgeOrder =
        CreateOrderAcknowledgmentLetter -> SendOrderAcknowledgment -> PricedOrder -> OrderAcknowledgementSent option

    let acknowledgeOrder: AcknowledgeOrder =
        fun createOrderAcknowledgmentLetter sendOrderAcknowledgment pricedOrder ->
            let letter = createOrderAcknowledgmentLetter pricedOrder
            let emailAddress = pricedOrder.CustomerInfo.EmailAddress

            let acknowledgement: OrderAcknowledgement =
                { EmailAddress = emailAddress
                  Letter = letter }

            let sendResult = sendOrderAcknowledgment acknowledgement

            match sendResult with
            | Sent ->
                Some
                    { OrderId = pricedOrder.OrderId
                      EmailAddress = emailAddress }
            | NotSent -> None

    let createBillingEvent (placedOrder: PricedOrder) : BillableOrderPlaced option =
        let orderId = placedOrder.OrderId
        let billingAddress = placedOrder.BillingAddress
        let amountToBill = placedOrder.AmountToBill

        if BillingAmount.value amountToBill > 0 then
            let order =
                { OrderId = orderId
                  BillingAddress = billingAddress
                  AmountToBill = amountToBill }

            Some order
        else
            None

    type CreateEvents = PricedOrder -> OrderAcknowledgementSent option -> PlaceOrderEvent list

    type ListOfOption<'a> = 'a option -> 'a list

    let listOfOption: ListOfOption<'a> =
        fun opt ->
            match opt with
            | Some value -> [ value ]
            | None -> []

    let createEvents: CreateEvents =
        fun pricedOrder acknowledgementSent ->
            let billingEvent = createBillingEvent pricedOrder
            let orderPlaced = OrderPlaced pricedOrder
            let billableOrderPlaced = Option.map BillableOrderPlaced billingEvent
            let acknowledgeSent = Option.map AcknowledgementSent acknowledgementSent

            [ yield orderPlaced
              yield! listOfOption acknowledgeSent
              yield! listOfOption billableOrderPlaced ]



    // type PlaceOrderWorkflow = PlaceOrder -> AsyncResult<PlaceOrderEvent list, PlaceOrderError>
    type PlaceOrderWorkflow = UnvalidatedOrder -> Result<PlaceOrderEvent list, PlaceOrderError>

    let placeOrder
        checkProductCodeExists
        checkAddressExists
        getProductPrice
        createOrderAcknowledgmentLetter
        sendOrderAcknowledgment
        : PlaceOrderWorkflow =
        fun unvalidatedOrder ->
            let serviceInfo =
                { Name = "AddressCheckingService"
                  Endpoint = "https://address-checking.service/api" }

            let validateOrder = validateOrder checkProductCodeExists checkAddressExists

            let priceOrder = priceOrderAdapted getProductPrice

            let acknowledgeOrder =
                acknowledgeOrder createOrderAcknowledgmentLetter sendOrderAcknowledgment

            result {
                let! validatedOrder = unvalidatedOrder |> validateOrder
                let! pricedOrder = priceOrder validatedOrder
                let orderAcknowledgementSent = pricedOrder |> acknowledgeOrder
                return createEvents pricedOrder orderAcknowledgementSent
            }
