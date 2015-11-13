module String = Core.Std.String
module Command = Core.Std.Command
module In_channel = Core.Std.In_channel
module Out_channel = Core.Std.Out_channel


type lang = Yip | C | Java | Python | OCaml;;

let extension = function
    | Yip -> ".yip"
    | C -> ".c"
    | Java -> ".java"
    | Python -> ".py"
    | OCaml -> ".ml";;

let boilerplate filename x = function
    | Yip ->
"main = fun -> " ^ x
    | C ->
"int main() {
    int x = " ^ x ^ ";
    return x;
}"

    | Java ->
"public class " ^ filename ^ " {
    public static void main(String[] args) {
        int x = " ^ x ^ ";
        System.exit(x);
    }
}"

    | Python ->
"x = " ^ x ^ "
exit(x)"

    | OCaml ->
"let x = " ^ x ^ "in
exit x";;

let lang_list = [Yip; C; Java; Python; OCaml]

let run filename () =
    In_channel.read_all filename
    |> String.strip
    |> (fun x -> List.map (boilerplate filename x) lang_list)
    |> List.combine lang_list
    |> List.iter (fun (lang, prog) -> Out_channel.write_all (filename ^ (extension lang)) prog)

let () =
    Command.basic ~summary:"Generate test data for a language"
        Command.Spec.(empty +> anon ("filename" %: file))
        run
    |> Command.run
