import gleam/io
import gleam/option.{None, Some}
import glove

pub fn main() {
  let data_def =
    glove.DataDef(
      linkage: glove.Linkage(exported: False, section: None, secflags: None),
      name: "str",
      align: None,
      items: [
        #(glove.Byte, glove.Str("hello world")),
        #(glove.Byte, glove.Constant(0)),
      ],
    )

  let main_func =
    glove.Function(
      linkage: glove.public(),
      name: "main",
      arguments: [],
      return_ty: Some(glove.Word),
      blocks: [
        glove.Block(
          label: "@start",
          statements: [
            glove.Assign(
              glove.Temporary("r"),
              glove.Word,
              glove.Call(
                glove.Global("puts"),
                [#(glove.Long, glove.Global("str"))],
              ),
            ),
            glove.Volatile(glove.Ret(Some(glove.Const(0)))),
          ],
        ),
      ],
    )

  let empty_module = glove.Module(functions: [], types: [], data: [])
  empty_module
  |> glove.add_data(data_def)
  |> glove.add_function(main_func)
  |> glove.display_module
  |> io.print
}
// Expectable result:
//export function w $main() {
//@start
//        # Call the puts function with $str as argument.
//        %r =w call $puts(l $str)
//        ret 0
//}
//
//data $str = { b "hello world", b 0 }
