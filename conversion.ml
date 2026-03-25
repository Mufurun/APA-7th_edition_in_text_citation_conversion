


(*
This program will convert reference list to the APA 7th edition intext citation and replace the in-text id with the formatted citation. 

NOTE: 
  - please sort the order of the reference before the application. 
  - same as the in-text citations.
  - FOLLOW THE EXACT FORMAT


Files: 
    - ref.txt   : file containing references
    - text.txt  : file of the main text 

    - out_put   : output file


For example: 

      <<<  ref.text  >>>

214: Persad, L. S., Binder-Markey, B. I., Shin, A. Y., Lieber, R. L., & Kaufman, K. R. (2023). American Society of Biomechanics Journal of Biomechanics Award 2022: Computer models do not accurately predict human muscle passive muscle force and fiber length: Evaluating subject-specific modeling impact on musculoskeletal model predictions. Journal of Biomechanics, 159, Article 111798. https://doi.org/10.1016/j.jbiomech.2023.111798
	https://search.library.uvic.ca/permalink/01VIC_INST/1d1fbv0/cdi_proquest_journals_2869637399

215: Valente, G., Pitto, L., Testi, D., Seth, A., Delp, S. L., Stagni, R., Viceconti, M., & Taddei, F. (2014). Are Subject-Specific Musculoskeletal Models Robust to the Uncertainties in Parameter Identification? PloS One, 9(11), e112625. https://doi.org/10.1371/journal.pone.0112625
	https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0112625&type=printable


      <<<   text.txt   >>>

Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. Some text here (214).
Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. 
Some text here. Some text here. Some text here. Some text here. Some text here (215; 214). Some text here. Some text here. 


      <<<   output.txt   >>>

Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. Some text here (Persad et al., 2023).
Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. Some text here. 
Some text here. Some text here. Some text here. Some text here. Some text here (Valente et al., 2014; Persad et al., 2023). Some text here. Some text here. 

*)


(*Reading file provided in a1*)
let read_file path =
  let fp = open_in  path in
  let s = really_input_string fp (in_channel_length fp) in
  close_in fp;
  s



let ref = read_file "ref.txt";; 
let text = read_file "text.txt";;


(*
steps: 

  1. split into paragraphs, then words. 
  
  2. get ID 
      - assume id followed by ":"

  3. get last_names and store in the list
      - assume last_name followed by "," before year
      - other initials followed by ".," or "."

  4. get year and end the iteration
      - assume id followed by ")."

  - everything in the list Options, and lines that is not applicable are None
  - If either id, name, or year has blank, then it fill with None. 

*)

let get_author_year text = 
  let list_refs = String.split_on_char '\n' text in 
  let rec each_ref li acc= 
    match li with 
    |[]-> acc
    |x :: xs->
      let list_of_words = String.split_on_char ' ' x in 

      let rec find_name_year l acc = 
        match acc with 
        |None-> None 
        |Some (id, name, year)-> (
          match l with 
          |x::xs -> (
            (*id*)
            if String.ends_with  ~suffix:":" x then 
              let num = int_of_string_opt (List.hd (String.split_on_char ':' x)) in 
              find_name_year xs (Some (num, name, year))
            (*First name initial annoying :(((( *)
            else if String.ends_with  ~suffix:".," x  then
              find_name_year xs acc
            (*Last name *)
            else if String.ends_with  ~suffix:"," x  then
              let this_name = List.hd (String.split_on_char ',' x) in
              match name with 
              |None-> find_name_year xs (Some (id, Some ([this_name]), year))
              |Some name_list -> find_name_year xs (Some (id, Some (name_list@[this_name]), year))
            (*year*)
            else if String.ends_with  ~suffix:")." x then
              let this_year = int_of_string_opt (String.sub x 1 4) in
              Some (id, name, this_year)
            (* everything else*)
            else find_name_year xs acc
            ) 
          |[] -> match id with |None -> None |_-> Some  (id, name, None)
          )in 
        each_ref xs (acc@[find_name_year list_of_words (Some (None, None, None))])
      in 
  each_ref list_refs [];;
    


(*
Convert taken list into List of id and in-text citation
      format: Some (id, Some "names, year")

      This is option.  
        - Tuples can be None
        - String can be None if year is not provided (presumably, the previous step was wrong)

*)


let in_text lst =
  let rec decoder l acc =
  match l with
  |[]-> acc
  |x::xs ->( 
    match  x with  
    |None-> decoder xs acc
    |Some (Some id, Some names, Some year) -> 
      let str =  
       if List.length names > 2 then  (List.hd names)^" et al., "^ (string_of_int year)
       else if List.length names = 2 then  (List.hd names)^" & "^ (List.hd (List.tl names))^ ", "^ (string_of_int year) 
       else  (List.hd names)^", "^ (string_of_int year)  in 
       decoder xs (acc @ [(id, Some str)])
     |Some (Some id, _, _) -> decoder xs (acc@[id, None])
     |Some _ -> decoder xs acc
     )
    in
    decoder lst []
;;


(*
Given id and list of (id, "citation") tuples, return the "citation"

  - If "" is None, that is the error from the previous step
      - indicating the error
  - if you cannot find the id, return the string of id 
      - In the next step, it can feed year or some number inside the paransis so just return the string.
*)
let rec look_up id lst = 
  match lst with
  |(i, Some(st))::xs-> if i = id then st else look_up id xs
  |(i, None)::xs -> if i = id then "Error_in_id: " ^ (string_of_int id) else look_up id xs
  |_-> string_of_int id;;


(*
Replace the id in text.txt file with the in-text citation in ref.txt file

  Steps:

    1. separate the text by "(" 

    2. check if the segment has ")" to ignore the first one. 
        - then, separate it by ")" and ignore the outside of the paransis

    3. find the citation id (presumably followed by ";" or ",")
        - replace the id with in-text citation using look_up
        - connect then by "; " and ending with ""


*)

  let replace_citation text lst =
    let without_l = String.split_on_char '(' text in 

    let rec citation_st l acc= 
      match l with
      |[]-> acc
      |x::[]-> let id = int_of_string_opt x in
        if id = None then acc ^ x else 
          (look_up (Option.get id) lst)
      |x::xs-> (
        let id =  int_of_string_opt x in
        if id != None then citation_st xs (acc ^ (look_up (Option.get id) lst))
        else 
          let y = List.hd (String.split_on_char ';' x) in 
          let id =  int_of_string_opt y in
        if id != None then citation_st xs (acc ^ (look_up (Option.get id) lst) ^ "; ")
        else 
          let z = List.hd (String.split_on_char ',' y) in 
          let id =  int_of_string_opt z in
        if id != None then citation_st xs (acc ^ (look_up (Option.get id) lst) ^ "; ")
        else citation_st xs (acc ^ x ^" ")
      )
    in
    let rec get_inside l acc = 
      match l with
      |[]-> acc
      |x::xs-> (
        if not (String.contains x ')' ) then
          get_inside xs (acc ^ x ^ "(")
        else 
          let inside = String.split_on_char ')' x in
          let cites = String.split_on_char ' ' (List.hd inside) in
          get_inside xs (acc ^ (citation_st cites "") ^")"^ List.hd (List.tl inside) ^ (if xs = [] then "" else "("))
      ) in
    get_inside without_l "";; 



(*
Return the value in the out_put.txt
*)

let () =
  let filename = "output.txt" in
  let content = replace_citation text (in_text (get_author_year ref)) in

  (* Open file for writing *)
  let oc = open_out filename in

  (* Write the string to the channel *)
  Out_channel.output_string oc content;

  (* Close the channel to flush the buffer and free resources *)
  close_out oc;;



(*

##### Debugging Tools ######

1: make a list of in text citation in the format: 
      id: name, year


let to_string lst = 
  let rec sum lst ret  = 
    match lst with 
    |[]-> ret
    |(id, Some cite)::xs-> sum xs (ret ^ string_of_int id^": " ^cite ^ "\n")
    |(id, None)::xs -> sum xs (ret ^ string_of_int id ^ ": Error for this reference\n")
  in
  sum lst "";;



print_endline(to_string (in_text (get_author_year ref)));;


print_endline(look_up 100 (in_text (get_author_year ref)));;




*)
