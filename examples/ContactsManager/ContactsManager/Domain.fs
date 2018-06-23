namespace Domain

type NumberType =
    | Mobile
    | Work
    | Home
    | Fax
    | Other of string
with
    override this.ToString () =
        match this with
        | Other t -> t
        | _ -> sprintf "%A" this

    static member Choices = [
        Mobile
        Work
        Home
        Fax
    ]

module Format =
    let phone (number: string) =
        let insert c i (s: string) =
            s.Substring(0, i) + c + s.Substring(i)
        if number.Length = 10 then
            number |> insert "-" 6 |> insert ") " 3 |> insert "(" 0
        else if number.Length = 7 then
            number |> insert "-" 3
        else number

[<CLIMutable>]
type ContactNumber = {
    Number: string
    Type: NumberType
} with
    override this.ToString () =
        sprintf "%s: %s" (string this.Type) (Format.phone this.Number)

type Contact = {
    FirstName: string
    LastName: string
    Group: string option
    Numbers: ContactNumber list
    Notes: string
} with
    member this.FullName =
        sprintf "%s %s" this.FirstName this.LastName

    member this.PrimaryNumber =
        this.Numbers |> List.tryHead |> Option.map (string)


open System.Windows.Data

type NumberTypeConverter() =
    interface IValueConverter with
        member this.Convert (value, _, _, _) =
            match value :?> NumberType with
            | Other s -> s |> box
            | t -> sprintf "%A" t |> box

        member this.ConvertBack (value, _, _, _) =
            let typ = value :?> string
            let case =
                Reflection.FSharpType.GetUnionCases(typeof<NumberType>)
                |> Seq.tryFind (fun c -> c.Name.ToLower() = typ.ToLower())
            match case with
            | Some c -> Reflection.FSharpValue.MakeUnion(c, [||])
            | None -> Other typ |> box
