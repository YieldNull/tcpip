open Core

module Bytes = Caml.Bytes

let set_int buf ~pos ~len value =
  for i = 0 to len - 1 do
    let value = value lsr (8 * (len - 1 - i)) land 0xff in
    Bytes.set buf (pos + i) (char_of_int value)
  done

let set_ipv4 buf ~pos value =
  let len = 4 in
  for i = 0 to len - 1 do
    let value = Int32.to_int_exn @@
      Int32.(land) (Int32.(lsr) value (8 * (len - 1 - i))) 0xffl
    in
    Bytes.set buf (pos + i) (char_of_int value)
  done

let set_byte_arr buf ~pos arr =
  List.iteri arr ~f:(fun i b -> Bytes.set buf (pos + i) (char_of_int b))

let set_bytes buf ~pos data =
  Bytes.blit data 0 buf pos (Bytes.length data)

let int_of_bytes data =
  let len = Bytes.length data in
  String.foldi data ~init:0 ~f:(fun i acc chr ->
      let num = int_of_char chr in
      num lsl ((len - i - 1) * 8) + acc
    )

let carry_and_lnot sum =
  let sum = ref sum in
  while !sum lsr 16 > 0 do
    sum := !sum land 0xffff + !sum lsr 16
  done;
  (lnot !sum) land 0xffff

let one's_sum packet =
  let sum = ref 0 in
  let len = Cstruct.len packet in
  for i = 0 to len - 1 do
    if i mod 2 = 1 then
      let high = Cstruct.get_uint8 packet (i - 1) in
      let low = Cstruct.get_uint8 packet i in
      let num = high lsl 8 + low in
      sum := !sum + num;
  done;
  if len mod 2 = 1 then begin
    let high = Cstruct.get_uint8 packet (len - 1) in
    let num = high lsl 8 in
    sum := !sum + num
  end;
  !sum

let checksum packet = carry_and_lnot @@ one's_sum packet

let validate packet = checksum packet = 0

let checksum_list packets =
  carry_and_lnot (List.fold packets ~init:0 ~f:(fun acc p -> one's_sum p + acc))

let validate_list packets = checksum_list packets = 0

let send_bytes ?pos ?len frame =
  let pos = Option.value pos ~default:0 in
  let len = Option.value len ~default:(Bytes.length frame) in
  ignore @@ Unix.write Iface.fd ~buf:frame ~pos ~len
