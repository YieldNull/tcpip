open Core

let set_int buf ~pos ~len value =
  for i = 0 to len - 1 do
    let value = value lsr (8 * (len - 1 - i)) land 0xff in
    String.set buf (pos + i) (char_of_int value)
  done

let set_ipv4 buf ~pos value =
  let len = 4 in
  for i = 0 to len - 1 do
    let value = Int32.to_int_exn @@
      value Int32.(lsr) (8 * (len - 1 - i)) Int32.(land) 0xffl
    in
    String.set buf (pos + i) (char_of_int value)
  done

let set_byte_arr buf ~pos arr =
  List.iteri arr ~f:(fun i b -> String.set buf (pos + i) (char_of_int b))

let set_bytes buf ~pos data =
  String.blit ~src:data ~src_pos:0 ~dst:buf ~dst_pos:pos ~len:(String.length data)

let int_of_bytes data =
  let len = String.length data in
  String.foldi data ~init:0 ~f:(fun i acc chr ->
      let num = int_of_char chr in
      num lsl ((len - i - 1) * 8) + acc
    )

let checksum data =
  let sum = ref 0 in
  let len = String.length data in
  for i = 0 to len - 1 do
    if i mod 2 = 1 then
      let high = int_of_char @@ String.get data (i - 1) in
      let low = int_of_char @@ String.get data i in
      let num = high lsl 8 + low in
      sum := !sum + num;
  done;
  if len mod 2 = 1 then begin
    let high = int_of_char @@ String.get data (len - 1) in
    let num = high lsl 8 in
    sum := !sum + num
  end;
  while !sum lsr 16 > 0 do
    sum := !sum land 0xffff + !sum lsr 16
  done;
  lnot !sum
