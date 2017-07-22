﻿module DataBindingTests

open System
open NUnit.Framework
open FsUnit
open FsUnitTyped
open VinylUI
open BindingPatterns

type Control() =
    member val Text = "1" with get, set
    member val ObjValue = box 1 with get, set
    member val Value = Nullable(1) with get, set
    static member TextProperty = typedefof<Control>.GetProperty("Text")
    static member ObjValueProperty = typedefof<Control>.GetProperty("ObjValue")
    static member ValueProperty = typedefof<Control>.GetProperty("Value")

type Form = {
    MyControl: Control
}

type Model = {
    Name: string
    Number: int
    Age: int option
}
with
    static member NameProperty = typedefof<Model>.GetProperty("Name")
    static member NumberProperty = typedefof<Model>.GetProperty("Number")
    static member AgeProperty = typedefof<Model>.GetProperty("Age")

let control = Control()
let form = { MyControl = control }
let model = { Name = "tim"; Number = 2; Age = Some 25 }

let bindInfo controlProp sourceProp hasConverter updateMode =
    let converter = if hasConverter then Some { ToControl = id; ToSource = id } else None
    { Control = control :> obj; ControlProperty = controlProp; Source = model; SourceProperty = sourceProp
      Converter = converter; UpdateMode = updateMode }

let bindExpression expr =
    match expr with
    | BindExpression bi -> bi
    | _ -> failwithf "expr did not parse: %A" expr

let bindInfoMatches expected actual =
    actual.Control |> shouldEqual expected.Control
    actual.ControlProperty |> shouldEqual expected.ControlProperty
    actual.Source |> shouldEqual expected.Source
    actual.SourceProperty |> shouldEqual expected.SourceProperty
    actual.Converter.IsSome |> shouldEqual expected.Converter.IsSome
    actual.UpdateMode |> shouldEqual expected.UpdateMode

[<Test>]
let ``BindExpression parses set control property to model property``() =
    <@ control.Text <- model.Name @> |> bindExpression
    |> bindInfoMatches (bindInfo Control.TextProperty Model.NameProperty false None)

[<Test>]
let ``BindExpression parses set form's control property to model property``() =
    <@ form.MyControl.Text <- model.Name @> |> bindExpression
    |> bindInfoMatches (bindInfo Control.TextProperty Model.NameProperty false None)

[<Test>]
let ``BindExpression parses set control obj property to model property``() =
    <@ control.ObjValue <- model.Number :> obj @> |> bindExpression
    |> bindInfoMatches (bindInfo Control.ObjValueProperty Model.NumberProperty false None)

[<Test>]
let ``BindExpression parses set control obj property to model option property, uses converter``() =
    let bi = <@ control.ObjValue <- model.Age @> |> bindExpression
    bi |> bindInfoMatches (bindInfo Control.ObjValueProperty Model.AgeProperty true None)
    let conv = bi.Converter.Value
    conv.ToControl null |> should equal null
    conv.ToControl (Some 1 :> obj) |> should equal 1
    conv.ToSource null |> should equal null
    conv.ToSource (1 :> obj) |> should equal (Some 1)

[<Test>]
let ``BindExpression parses set control obj property to model option property with explicit converter``() =
    let bi = <@ control.ObjValue <- model.Age |> Option.toNullable @> |> bindExpression
    bi |> bindInfoMatches (bindInfo Control.ObjValueProperty Model.AgeProperty true None)

[<Test>]
let ``BindExpression parses set control nullable property to model property, uses converter``() =
    let bi = <@ control.Value <- model.Number |> Nullable @> |> bindExpression
    bi |> bindInfoMatches (bindInfo Control.ValueProperty Model.NumberProperty true None)
    let conv = bi.Converter.Value
    conv.ToControl (1 :> obj) |> should equal 1
    conv.ToSource null |> should equal 0
    conv.ToSource (Nullable 1 :> obj) |> should equal 1

[<Test>]
let ``BindExpression parses set control nullable property to model option property, uses converter``() =
    let bi = <@ control.Value <- model.Age |> Option.toNullable @> |> bindExpression
    bi |> bindInfoMatches (bindInfo Control.ValueProperty Model.AgeProperty true None)
    let conv = bi.Converter.Value
    conv.ToControl null |> should equal null
    conv.ToControl (Some 1 :> obj) |> should equal 1
    conv.ToSource null |> should equal null
    conv.ToSource (1 :> obj) |> should equal (Some 1)

[<Test>]
let ``BindExpression parses set control property to model property with update on changed``() =
    <@ control.Text <- model.Name |> BindOption.UpdateSourceOnChange @> |> bindExpression
    |> bindInfoMatches (bindInfo Control.TextProperty Model.NameProperty false (Some OnChange))

[<Test>]
let ``BindExpression parses set control property to model property with update never``() =
    <@ control.Text <- model.Name |> BindOption.UpdateSourceNever @> |> bindExpression
    |> bindInfoMatches (bindInfo Control.TextProperty Model.NameProperty false (Some Never))