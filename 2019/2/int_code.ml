let rec run : int array -> int -> unit =
 fun data pos ->
  let opcode = data.(pos) in
  match opcode with
  | 1 ->
      let src1 = data.(data.(pos + 1))
      and src2 = data.(data.(pos + 2))
      and dst = data.(pos + 3) in
      data.(dst) <- src1 + src2;
      run data (pos + 4)
  | 2 ->
      let src1 = data.(data.(pos + 1))
      and src2 = data.(data.(pos + 2))
      and dst = data.(pos + 3) in
      data.(dst) <- src1 * src2;
      run data (pos + 4)
  | 99 -> ()
  | _ -> failwith "invalid opcode"
