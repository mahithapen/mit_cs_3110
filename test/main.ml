(******************************************************************************* 

  Test plan

  We approached our testing by ensuring that each of our foundational modules
  functioned effectively and incorporating a variety of strategies to give us a
  robust and logical test suite:

  1. Modules that were test: 
  - Blob: File reading within a commit, content hashing, differences 
  - Tree: Proper tree creation from a directory and parsing through a tree 
  - Commit: Validate commit creation, parameter setting, parent linkage 
  - Param: Properly parsing command line arguments and exceptions 
  - Hash: Creating unique hashes for our commits 
  - Reader: Reading the file content 
  - Writer: Writing to a file

  2. We used an automated testing approach and functors throughout our tests. We
  wanted to ensure that we tested each module individually so that we could
  ensure that our system would work as well. We also went through the modules to
  ensure that each operation was tested efficiently. We also ensured that our
  modules such as commit and tree were interacting properly through the use of
  our tests. Finally we incorporated a type of regression testing with
  post-modifications to ensure new changes do not break existing
  functionalities.

  3. We incorporated both BlackBox and GlassBox testing as we wanted to check
  whether realistic logical paths were covered based on our code and that the
  functions themselves worked as expected. This was important as it helped us
  iterate and create the exceptions. We also have randomized testing to
  strengthen our general tests as we wanted to make sure many inputs would not
  break our functionality. The use of our two testing strategies helped us
  ensure that our tests were robust and logical.


 *****************************************************************************)

open OUnit2
open Src
open Reader
open Blob
open Tree
open Writer
open Commit
open Cmd
open Param

(** [index_test name input expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with [index input]. *)
let string_of_string s = s

let test_dir = "test/test-files/"

let read_file_test (expected_output : string) (input : string) (name : string) :
    test =
  name >:: fun _ ->
  let result = read_file input in
  assert_equal expected_output result ~printer:string_of_string
(* Functions defined for testing file modification*)

let write_to_file_test (input : string) (data : string) (name : string) : test =
  name >:: fun _ ->
  let expected_output =
    match read_file_body input with
    | exception End_of_file -> data
    | s -> (
        match (s, data) with
        | s, "\n" -> s ^ data
        | s, _ -> s ^ "\n" ^ data)
  in
  let result =
    append_to_file input data;
    read_file_body input
  in
  assert_equal expected_output result ~printer:string_of_string

(** Helper function appending directory of test files*)
let ( ~/ ) (s : string) = test_dir ^ s

let read_file_tests =
  [
    "contributors testing"
    |> read_file_test
         "Line 1: Test File\n\
          Line 2: Sia\n\
          Line 3: Nidhi\n\
          Line 4: Mahitha\n\
          Line 5: Michael\n\
          END OF FILE" ~/"contributors.txt";
    "emptyLines testing"
    |> read_file_test
         "Line 1:   \n\
          Line 2:   \n\
          Line 3:   \n\
          Line 4:   \n\
          Line 5:   \n\
          END OF FILE" ~/"emptyLines.txt";
    "empty testing" |> read_file_test "END OF FILE" ~/"empty.txt";
    "emptyLinesEnd testing"
    |> read_file_test
         "Line 1: 17346\n\
          Line 2: \n\
          Line 3: \n\
          Line 4: $$$\n\
          Line 5: \n\
          Line 6: \n\
          Line 7: true456\n\
          Line 8: \n\
          Line 9:  \n\
          END OF FILE" ~/"emptyLineEnd.txt";
    "emptyLinesBeg testing"
    |> read_file_test
         "Line 1: \nLine 2: \nLine 3: \nLine 4: Hi\nLine 5: Hello\nEND OF FILE"
         ~/"emptyLineBeg.txt";
    "oneLiner testing"
    |> read_file_test "Line 1: Hello\nEND OF FILE" ~/"oneLiner.txt";
    "FileNotFound testing"
    |> read_file_test "ERROR: File 'test/test-files/fakeFile.txt' not found."
         ~/"fakeFile.txt";
  ]

let write_to_file_test1 =
  let ( ~/ ) (s : string) = test_dir ^ s in
  [ "courses appending" |> write_to_file_test ~/"forWriter.txt" "CS1110" ]

let write_to_file_test2 =
  let ( ~/ ) (s : string) = test_dir ^ s in
  [ "append\n another course" |> write_to_file_test ~/"forWriter.txt" "CS2110" ]

let write_to_file_test3 =
  let ( ~/ ) (s : string) = test_dir ^ s in
  [ "add empty line" |> write_to_file_test ~/"forWriter.txt" "\n" ]

(* START OF TESTING FOR COMMIT *)

(** OUnit test named [name] asserting the equality of [output] and [expected] *)
let eq expected output (name : string) =
  name >:: fun _ -> assert_equal expected output

let eq_string expected output (name : string) =
  name >:: fun _ -> assert_equal expected output ~printer:(fun x -> x)

(** OUnit test named [name] asserting the non-equality of [output] and
    [expected] *)
let not_eq expected output (name : string) =
  name >:: fun _ ->
  assert_equal false (expected = output) ~printer:string_of_bool

let fails exc exp name = name >:: fun _ -> assert_raises exc exp

module BlobTest (B : BlobType) = struct
  let b1 = B.of_file ~/"contributors.txt"
  let b1_diff_path = B.of_file ~/"more-files/contributors.txt"
  let b2 = B.of_file ~/"oneLiner.txt"
  let b2_copy = B.of_file ~/"oneLiner.txt"
  let b3 = B.of_file ~/"nonexistentFile.txt"
  let c1 = "Test File\nSia\nNidhi\nMahitha\nMichael"
  let b1_diff = B.of_file ~/"test_diff.txt"
  let b2_diff = B.of_file ~/"test_diff2.txt"
  let b3_diff = B.of_file ~/"test_diff*.txt"
  let b4_diff = B.of_file ~/"test_diff2*.txt"

  let diff_test name blob1 blob2 expected_output =
    name >:: fun _ ->
    let output = B.diff blob1 blob2 in
    (* Convert the output list to a single string *)
    let output_str = String.concat "\n" output in
    assert_equal expected_output output_str ~printer:(fun x -> x)

  let diff_test_same name blob1 blob2 expected_output =
    name >:: fun _ ->
    let output = B.diff blob1 blob2 in
    (* Convert the output list to a single string *)
    let output_str = String.concat "\n" output in
    assert_equal expected_output output_str ~printer:(fun x -> x)

  let issame_test_true =
    "test_blob_issame_true" >:: fun _ ->
    assert_equal true (B.same b1 b1) ~printer:string_of_bool

  let issame_test_false =
    "test_blob_issame_true" >:: fun _ ->
    assert_equal false (B.same b1 b2) ~printer:string_of_bool

  let expected_diff_output1 =
    "Modified at line 1: 'Hello this is my difference test file' -> 'Hello \
     this is my difference test '\n\
     Added at line 2: 'This is my new line'"

  let expected_diff_output_same = "No changes made"

  let tests =
    [
      "test_blob_empty_name" |> eq "" (B.filename B.empty);
      "test_blob_empty_content" |> eq "" (B.content B.empty);
      "test_blob_contributor_name" |> eq "contributors.txt" (B.filename b1);
      "test_blob_contributor_content" |> eq c1 (B.content b1);
      "test_blob_oneLiner_name" |> eq "oneLiner.txt" (B.filename b2);
      "test_blob_oneLiner_content" |> eq "Hello" (B.content b2);
      "test_blob_invalid_file" |> eq B.empty b3;
      "test_blob_same_file_hash" |> eq (B.hash b2) (B.hash b2_copy);
      "test_blob_diff_path_hash" |> eq (B.hash b1) (B.hash b1_diff_path);
      "test_blob_diff_file_hash" |> not_eq (B.hash b2) (B.hash b1);
      diff_test "Testing diff between b1_diff and b2_diff" b1_diff b2_diff
        expected_diff_output1;
      diff_test_same "Testing diff between b1_diff and b2_diff" b1_diff b1_diff
        expected_diff_output_same;
      issame_test_true;
      issame_test_false;
    ]
end

module TreeTest (M : MakeTree) (B : BlobType) = struct
  module Tr = M (B)

  let t1 = Tr.empty
  let t2 = Tr.of_dir ~/"test-dir"
  let t2_diff = Tr.of_dir ~/"test-dir2/test-dir"
  let t3 = Tr.of_dir ~/"more-files"
  let t4 = Tr.of_dir ~/"all-diff-test1"
  let t4_diff = Tr.of_dir ~/"all-diff-test2"
  let all_diff_expected_output = ""

  let all_differences_test name t1 t2 expected_output =
    name >:: fun _ ->
    let output = Tr.all_differences t1 t2 in
    (* Convert the output list to a single string *)
    let output_str = String.concat "\n" output in
    assert_equal all_diff_expected_output output_str ~printer:(fun x -> x)

  let issame_test_true_nonempty =
    "test_blob_issame_true" >:: fun _ ->
    assert_equal true (Tr.issame t2 t2) ~printer:string_of_bool

  let issame_test_true_empty =
    "test_blob_issame_true" >:: fun _ ->
    assert_equal true (Tr.issame t1 t1) ~printer:string_of_bool

  let issame_test_false =
    "test_blob_issame_true" >:: fun _ ->
    assert_equal false (Tr.issame t1 t2) ~printer:string_of_bool

  let c2 =
    "dir1\n\
    \  * dir3\n\
    \    * file5.txt\n\
    \  * file3.txt\n\
     dir2\n\
    \  * file4.txt\n\
     file1.txt\n\
     file2.txt"

  let diff_test name tr1 tr2 expected_output =
    name >:: fun _ ->
    let output = Tr.diff tr1 tr2 in
    (* Convert the output list to a single string *)
    let output_str = String.concat "\n" output in
    assert_equal expected_output output_str ~printer:(fun x -> x)

  let diff_test_same name tr1 tr2 expected_output =
    name >:: fun _ ->
    let output = Tr.diff tr1 tr2 in
    (* Convert the output list to a single string *)
    let output_str = String.concat "\n" output in
    assert_equal expected_output output_str ~printer:(fun x -> x)

  let expected_diff_output_same = ""

  (* Expected output for the diff test between t2 and t3 *)
  let expected_diff_output =
    "Added directory contributors.txt\n\
     Deleted file     * file5.txt\n\
     Deleted file   * dir3\n\
     Deleted file   * file3.txt\n\
     Deleted file   * file4.txt\n\
     Deleted directory dir1\n\
     Deleted directory dir2\n\
     Deleted directory file1.txt\n\
     Deleted directory file2.txt"

  let tests =
    [
      "test_tree_empty_name" |> eq "" (Tr.foldername t1);
      "test_tree_empty_write" |> eq_string "" (Tr.write_tree t1);
      "test_tree_name" |> eq "test-dir" (Tr.foldername t2);
      "test_tree_write" |> eq_string c2 (Tr.write_tree t2);
      "test_tree_hash_same_struc" |> eq (Tr.hash t2) (Tr.hash t2_diff);
      "test_tree_hash_diff" |> not_eq (Tr.hash t2) (Tr.hash t3);
      ( "test_tree_hash_diff" >:: fun _ ->
        assert_equal false (Tr.hash t2 = Tr.hash t3) ~printer:string_of_bool );
      diff_test "Testing diff function between t2 and t3" t2 t3
        expected_diff_output;
      "tree_test_list_of_filepaths empty tree" |> eq [] Tr.(t1 |> list_of_files);
      "tree_test_list_of_filepaths t2"
      |> eq
           [
             "test/test-dir/dir1/dir3/file5.txt";
             "test/test-dir/dir1/file3.txt";
             "test/test-dir/dir2/file4.txt";
             "test/test-dir/file1.txt";
             "test/test-dir/file2.txt";
           ]
           Tr.(t2 |> list_of_files);
      "tree_test_list_of_filepaths t3"
      |> eq [ "test/more-files/contributors.txt" ] Tr.(t3 |> list_of_files);
      diff_test_same "Testing diff function between t2 and t3" t2 t2
        expected_diff_output_same;
      issame_test_true_empty;
      issame_test_true_nonempty;
      issame_test_false;
    ]
end

module StringBlobTests = BlobTest (StringBlob)
module StringListTreeTests = TreeTest (MakeListTree) (StringBlob)

module StringParamTests = struct
  module SP = StringParam

  let p1 = SP.make None "required single arg"
  let p2 = SP.make (Some "file") "optional single arg"
  let p3 = SP.make (Some "file") "optional single arg"
  let command0 = []
  let command1 = [ "123"; "more" ]
  let command2 = [ "-file"; "video.mp4" ]
  let command3 = [ "123"; "-file"; "video.mp4" ]
  let command4 = [ "-file" ]

  let tests =
    [
      "required_not_optional" |> eq false (SP.optional p1);
      "optional_is_optional" |> eq true (SP.optional p2);
      "required_none_value" |> eq None (SP.val_opt p3);
      "no_arguments_anonomyous"
      |> fails (MissingArguments "expected 1 argument") (fun () ->
             SP.consume p1 command0);
      "no_arguments_optional" |> eq [] (SP.consume p2 command0);
      "no_arguments_optional2" |> eq [ "-file" ] (SP.consume p2 command4);
      "flag_not_conumed_returns_tail"
      |> eq [ "123"; "-file"; "video.mp4" ] (SP.consume p2 command3);
      "consume_anonymous" |> eq [ "more" ] (SP.consume p1 command1);
      "consume_anonymous_value"
      |> eq (Some "123")
           (ignore (SP.consume p1 command1);
            SP.val_opt p1);
      "consume_optional" |> eq [] (SP.consume p2 command2);
      "consume_optional_value"
      |> eq (Some "video.mp4")
           (ignore (SP.consume p2 command2);
            SP.val_opt p2);
      "consume_both" |> eq [] (command3 |> SP.consume p1 |> SP.consume p2);
      "help_anonymous_string" |> eq_string "<string>" (SP.help p1);
      "help_optional_string" |> eq_string "[--file <string>]" (SP.help p2);
    ]
end

module FlagTests = struct
  module F = Flag

  let p = F.make (Some "flag") "flag for flag"
  let command0 = []
  let command1 = [ "123"; "more" ]
  let command2 = [ "-flag"; "more" ]

  let tests =
    [
      "flag_no_flag_passed"
      |> fails FlagNeedsFlag (fun () -> F.make None "dsec");
      "flag_is_optional" |> eq true (F.optional p);
      "flag_consume_no_args" |> eq [] (F.consume p command0);
      "flag_consume_doesn't_match_returns_tail"
      |> eq [ "123"; "more" ] (F.consume p command1);
      "flag_consumes_correctly" |> eq [ "more" ] (F.consume p command2);
      "flag_help" |> eq_string "[--flag]" (F.help p);
    ]
end

module HashTests = struct
  let s1 = "hello world!"
  let s2 = "the Earth says, 'hello'!"
  let h1 = Hash.of_string s1
  let h2 = Hash.of_string s2

  let tests =
    [
      "different hashes" |> not_eq (Hash.to_hex h1) (Hash.to_hex h2);
      "different hashes 2" |> eq false (Hash.equal h1 h2);
      "different hashes 3" |> not_eq 0 (Hash.compare h1 h2);
      "same hashes"
      |> eq_string (Hash.to_hex h1) (Hash.to_hex (Hash.of_string s1));
      "same hashes 2" |> eq true (Hash.equal h1 h1);
      "same hashes 2" |> eq 0 (Hash.compare h1 h1);
      "rehash gets new hash" |> eq false (Hash.equal h1 (Hash.rehash s1 h1));
      "rehash gets new hash, not concatenates"
      |> eq false (Hash.equal (Hash.of_string (s1 ^ s1)) (Hash.rehash s1 h1));
      "same hash after hex"
      |> eq true (Hash.equal h1 (Hash.from_hex (Hash.to_hex h1)));
      "rehashing after and before hexing"
      |> eq true
           (Hash.equal
              (Hash.from_hex (Hash.to_hex (Hash.rehash s2 h1)))
              (Hash.rehash s2 (Hash.from_hex (Hash.to_hex h1))));
    ]
end

module MitTests = struct
  open Buffer

  let c = Sys.getcwd ()
  let p = ~/"mit-tests"

  let make_mit_test (com : unit -> unit) =
    Sys.chdir p;
    com ();
    Sys.chdir c

  let tests =
    [
      "Intialize and clean."
      |> eq ()
           (make_mit_test (fun () ->
                Mit.init ();
                Mit.clean ()));
      "Commit."
      |> eq ()
           (make_mit_test (fun () ->
                Mit.init ();
                Mit.commit "Michael" "first commit";
                Mit.clean ()));
      "Commit many times and log."
      |> eq ()
           (make_mit_test (fun () ->
                Mit.init ();
                Mit.commit "Michael" "first commit";
                Mit.commit "Sia" "second commit";
                Mit.commit "Mahi" "third commit";
                Mit.commit "Nidhi" "fourth commit";
                Mit.log ();
                Mit.clean ()));
      "Status and change."
      |> eq ()
           (make_mit_test (fun () ->
                Mit.init ();
                Mit.commit "Alienor" "weirdo";
                Mit.status ();
                Mit.change ();
                Mit.clean ()));
      "Create many branches."
      |> eq ()
           (make_mit_test (fun () ->
                Mit.init ();
                Mit.commit "Alienor" "weirdo";
                Mit.branch "branch2";
                Mit.branch "branch3";
                Mit.branch "branch4";
                Mit.clean ()));
      "Checkout."
      |> eq ()
           (make_mit_test (fun () ->
                Mit.init ();
                Mit.commit "Alienor" "weirdo";
                Mit.branch "branch2";
                Mit.checkout "branch2" false;
                Mit.checkout "main" false;
                Mit.checkout "new-one" true;
                Mit.clean ()));
      "File creation/deletion"
      |> eq ()
           (make_mit_test (fun () ->
                Mit.init ();
                Writer.mkdir "dir1";
                Writer.write_to_file "file1.txt" "hello_world";
                Mit.commit "Me" "first";
                Writer.append_to_file "file1.txt" "_you're_an_idiot";
                Writer.mkdir "dir2";
                Writer.write_to_file "dir2/file2.txt" "another one";
                Mit.status ();
                Mit.commit "Me" "second";
                Mit.change ();
                Mit.branch "branch2";
                Mit.checkout "branch2" false;
                Writer.rm "dir1";
                Writer.rm "file1.txt";
                Writer.write_to_file "dir2/file2.txt" "another what?\nnah";
                Mit.status ();
                Mit.commit "You" "third";
                Mit.log ();
                Mit.checkout "main" false;
                Mit.clean ()));
    ]
end

let tests =
  "test suite"
  >::: List.flatten
         [
           read_file_tests;
           write_to_file_test1;
           StringBlobTests.tests;
           write_to_file_test2;
           StringListTreeTests.tests;
           write_to_file_test3;
           StringParamTests.tests;
           FlagTests.tests;
           HashTests.tests;
           MitTests.tests;
         ]

let _ = run_test_tt_main tests
