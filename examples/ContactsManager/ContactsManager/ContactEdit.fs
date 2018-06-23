module ContactEdit

open System
open System.Windows
open VinylUI
open VinylUI.Wpf
open Domain

type Model = {
    FirstName: Result<string, string>
    LastName: Result<string, string>
    Groups: string list
    Group: string option
    Numbers: ContactNumber ResizeArray
    Notes: string
    Result: Contact option
}

type Events =
    | Save
    | Cancel

module Errors =
    let nameBlank typ = Error <| sprintf "%s name cannot be blank" typ

type View = FsXaml.XAML<"ContactEditWindow.xaml">

let binder (view: View) model =
    let nonEmpty typ s =
        if String.IsNullOrWhiteSpace s then Errors.nameBlank typ
        else Ok s

    view.NumberGrid.ItemsSource <- model.Numbers

    [ Bind.view(<@ view.FirstNameBox.Text @>).toModelResult(<@ model.FirstName @>, nonEmpty "First")
      Bind.view(<@ view.LastNameBox.Text @>).toModelResult(<@ model.LastName @>, nonEmpty "Last")
      Bind.model(<@ model.Groups @>).toItemsSource(view.GroupCombo)
      Bind.view(<@ view.GroupCombo.Text @>).toModel(<@ model.Group @>)
      Bind.view(<@ view.NotesBox.Text @>).toModel(<@ model.Notes @>)
    ]

let events (view: View) =
    [ view.SaveButton.Click |> Observable.mapTo Save
      view.CancelButton.Click |> Observable.mapTo Cancel
    ]

let save close model =
    match model.FirstName, model.LastName with
    | Ok first, Ok last ->
        let contact = 
            { FirstName = first
              LastName = last
              Group = model.Group
              Numbers = model.Numbers |> Seq.toList
              Notes = model.Notes
            }
        close ()
        { model with Result = Some contact }
    | Error e, _ | _, Error e ->
        MessageBox.Show(e, "Invalid Information", MessageBoxButton.OK, MessageBoxImage.Warning) |> ignore
        model

let dispatcher (close: unit -> unit) = function
    | Save -> Sync (save close)
    | Cancel -> Sync (fun m -> close(); m)

let start groups (contact: Contact option) (view: View) =
    let model =
        match contact with
        | Some c ->
            { FirstName = Ok c.FirstName
              LastName = Ok c.LastName
              Groups = groups
              Group = c.Group
              Numbers = c.Numbers |> ResizeArray
              Notes = c.Notes
              Result = None
            }
        | None ->
            { FirstName = Errors.nameBlank "First"
              LastName = Errors.nameBlank "Last"
              Groups = groups
              Group = None
              Numbers = ResizeArray()
              Notes = ""
              Result = None
            }
    Framework.start binder events (dispatcher view.Close) view model
