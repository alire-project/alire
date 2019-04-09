with Alire.Index.LibGNUTLS;
with Alire.Index.UnixODBC;

package Alire.Index.Simple_Components is

   --  Simple Components by Dmitry A. Kazakov

   Repo        : constant URL    := "https://github.com/alire-project/dak_simple_components.git";
   DAK_Author  : constant String := "Dmitry A. Kazakov";
   DAK_Website : constant String := "http://www.dmitry-kazakov.de/ada/components.htm";

   function Project is
     new Catalogued_Project ("Simple Components (root project)");
   --  This is a special project because it contains no code.

   --  NOTE: since all extensions reside in the same commit/folder, there's no need for interdependencies

   Base_V_4_27 : constant Release :=
                   Project.Unreleased
                     (V ("4.27"),
                      Git (Repo, "7cafd2da4a92cfe2b1a45374de6d35fc904b2788"),

                      Properties =>
                        GPR_Scenario ("Legacy", "Ada95" or "Ada2005" or "Ada2012") and
                        GPR_Scenario ("Development", "Debug" or "Release") and

                        License  (GMGPL_2_0) and
                        Author   (DAK_Author) and
                        Website  (DAK_Website));

   -----------------
   -- Connections --
   -----------------

   package Connections is

      function Project is new Catalogued_Project
        ("Simple Components (clients/servers)");

      V_4_27 : constant Release :=
                             Project.Register
                               (Base_V_4_27
                                                       --                               .Replacing (Git (Repo, "008935d5a89396cc0c39afb39f04bf6a89a92058"))
                                .Extending
                                  (
                                   --                                  Dependencies       =>
                                   --                                    Components_V_4_27.Within_Major and
                                   --                                    Sqlite_V_4_27.Within_Major,

                                   Properties         =>
                                   --  Main projects
                                     Project_File ("components-connections_server.gpr") and
                                     Project_File ("components-connections_server-elv_max_cube.gpr") and
                                     Project_File ("components-connections_server-http_server.gpr") and
                                     Project_File ("components-connections_server-http_server-sqlite_browser.gpr") and
                                     Project_File ("components-connections_server-modbus.gpr") and
                                     Project_File ("components-connections_server-mqtt.gpr") and
                                     Project_File ("components-connections_server-smtp.gpr"),

                                   Private_Properties =>
                                     Executable ("test_data_server") and
                                     Executable ("test_echo_client") and
                                     Executable ("test_echo_client_async") and
                                     Executable ("test_echo_server") and
                                     Executable ("test_elv_max_cube_client") and
                                     Executable ("test_http_client") and
                                     Executable ("test_http_continuous_server") and
                                     Executable ("test_http_server") and
                                     Executable ("test_infinity_server") and
                                     Executable ("test_modbus_client") and
                                     Executable ("test_mqtt_client") and
                                     Executable ("test_mqtt_server") and
                                     Executable ("test_mqtt_webserver") and
                                     Executable ("test_websocket_duplex_server") and
                                     Executable ("test_websocket_server") and

                                     Project_File ("test_components/components-connections_server-elv_max_cube-test_elv_max_cube_client.gpr") and
                                     Project_File ("test_components/components-connections_server-http_server-test_http_server.gpr") and
                                     Project_File ("test_components/components-connections_server-modbus-test_modbus_client.gpr") and
                                     Project_File ("test_components/components-connections_server-mqtt-test_mqtt.gpr") and
                                     Project_File ("test_components/components-connections_server-test_data_server.gpr") and
                                     Project_File ("test_components/components-connections_server-test_echo_client_async.gpr") and
                                     Project_File ("test_components/components-connections_server-test_echo_client.gpr") and
                                     Project_File ("test_components/components-connections_server-test_echo_server.gpr") and
                                     Project_File ("test_components/components-connections_server-test_websockets_mqtt.gpr") and

                                   --  Those fail for some reason on missing libdl
                                     On_Condition (Compiler > GNAT_FSF_7_3_Or_Newer,
                                       Executable ("test_http_sqlite_browser") and
                                         Project_File ("test_components/components-test_sqlite_browser.gpr"))
                                  ));

      ---------
      -- NTP --
      ---------

      package NTP is

         function Project is new Catalogued_Project
           ("Simple Components (Network Time Protocol)");

         V_4_27 : constant Release :=
                        Project.Register
                          (Base_V_4_27
                                          --                       .Replacing (Origin => Git (Repo, "34fb305d6ef360cde5e272b51409097a5de72017"))
                           .Extending
                             (
                              --                          Dependencies =>
                              --                            Components_V_4_27.Within_Major,

                              Properties         =>
                                Project_File ("components-ntp.gpr"),

                              Private_Properties =>
                                Executable ("test_ntp") and
                                Project_File ("test_components/components-ntp-test_ntp.gpr")));

      end NTP;

      ------------
      -- Secure --
      ------------

      package Secure is

         function Project is new Catalogued_Project
           ("Simple Components (clients/servers over TLS)");

         V_4_27 : constant Release :=
                     Project.Register
                       (Base_V_4_27
--                          .Replacing (Origin => Git (Repo, "ca72cf4150ae14ba6d40c3d2dd92c7846cb4cb5d"))
                        .Extending
                          (Dependencies       =>
--                               Connections_V_4_27.Within_Major and
                             LibGNUTLS.V_3_5_8.Within_Major,

                           Properties         =>
                             Project_File ("components-connections_server-secure.gpr") and
                             Project_File ("components-gnutls.gpr"),

                           Private_Properties =>
                             Executable ("test_https_client") and
                             Executable ("test_https_server") and
                             Executable ("test_smtp_client") and

                             Project_File ("test_components/components-connections_server-http_server-test_https_server.gpr") and
                             Project_File ("test_components/components-connections_server-smtp-test_smtp.gpr")));

      end Secure;

   end Connections;

   ----------
   -- Core --
   ----------

   package Core is

      function Project is new Catalogued_Project
        ("Simple Components (core components)");

      Components_V_4_27 : constant Release :=
                            Project.Register
                              (Base_V_4_27
                                                     --                              .Replacing (Origin => Git (Repo, "542f02c9be86693f759fcb784a8462bc4b25f1f2"))
                               .Extending
                                 (
                                  --                                 Dependencies =>
                                  --                                   Strings_Edit_V_4_27.Within_Major and
                                  --                                   Tables_V_4_27.Within_Major,
                                  Properties         =>
                                    Project_File ("components.gpr") and

                                    GPR_Scenario ("Atomic_Access", "Pragma-atomic" or "GCC-built-ins" or "GCC-long-offsets") and
                                    GPR_Scenario ("Tasking", "Multiple" or "Single") and
                                    GPR_Scenario ("Traced_objects", "Off" or "On") and

                                    Comment  ("Tasking=Single seems to be broken at persistent-single_file-text_io.adb"),

                                  Private_Properties =>
                                    Executable ("test_approximations") and
                                    Executable ("test_association") and
                                    Executable ("test_blackboard") and
                                    Executable ("test_blackboard_performance") and
                                    Executable ("test_blocking_files") and
                                    Executable ("test_block_streams") and
                                    Executable ("test_b_trees") and
                                    Executable ("test_cubic_spline") and
                                    Executable ("test_dining_philosophers") and
                                    Executable ("test_fifo") and
                                    Executable ("test_generic_indefinite_sets") and
                                    Executable ("test_generic_maps") and
                                    Executable ("test_generic_sets") and
                                    Executable ("test_graphs") and
                                    Executable ("test_handles") and
                                    Executable ("test_ieee_754") and
                                    Executable ("test_linked_lists") and
                                    Executable ("test_linked_lists_scheduler_test") and
                                    Executable ("test_parser_stream_io") and
                                    Executable ("test_persistent_memory_pool") and
                                    Executable ("test_persistent_storage") and
                                    Executable ("test_sequencer") and
                                    Executable ("test_single_file_persistence") and
                                    Executable ("test_stack") and
                                    Executable ("test_storage_streams") and
                                    Executable ("test_string_streams") and
                                    Executable ("test_synchronization_events") and
                                    Executable ("test_transactional_blocking_files") and
                                    Executable ("test_utf8_tables") and

                                    Project_File ("test_components/components-tests.gpr")));

   end Core;

   ----------
   -- ODBC --
   ----------

   package ODBC is

      function Project is new Catalogued_Project
        ("Simple Components (ODBC bindings)");

      ODBC_V_4_27 : constant Release :=
                      Project.Register
                        (Base_V_4_27
                                         --                        .Replacing (Origin => Git (Repo, "47337f8a5dd69404087129d5cca79885d6e8cd3f"))
                         .Extending
                           (Dependencies       =>
                            --                             Components_V_4_27.Within_Major and
                              UnixODBC.V_2_3.Within_Major,

                            Properties         =>
                              Project_File ("components-odbc.gpr"),

                            Private_Properties =>
                              Executable ("test_odbc_bindings") and

                              Project_File ("test_components/components-odbc-odbc_bindings_tests.gpr") and

                              On_Condition
                                (Operating_System = GNU_Linux,
                                 GPR_External ("odbc", "unixODBC")) and
                              On_Condition
                                (Operating_System = Windows,
                                 GPR_External ("odbc", "ODBC32")) and
                              On_Condition
                                (Word_Size = Bits_32,
                                 GPR_External ("arch", "i686")) and
                              On_Condition
                                (Word_Size = Bits_64,
                                 GPR_External ("arch", "x86_64"))));

   end ODBC;

   ------------
   -- Sqlite --
   ------------

   package Sqlite is

      function Project is new Catalogued_Project
        ("Simple Components (SQLite)");

      Sqlite_V_4_27 : constant Release :=
                        Project.Register
                          (Base_V_4_27
                                             --                          .Replacing (Origin => Git (Repo, "6fda0f3f7494815c87b329f7411b9a49ff97b9ba"))
                           .Extending
                             (
                              --                             Dependencies       =>
                              --                               Components_V_4_27.Within_Major,

                              Properties         =>
                                Project_File ("components-sqlite.gpr"),

                              Private_Properties =>
                              --  Those fail for some reason on missing libdl
                                On_Condition (Compiler > GNAT_FSF_7_3_Or_Newer,
                                  Executable ("test_sqlite_benchmark") and
                                    Executable ("test_sqlite_persistence") and

                                    Project_File ("test_components/components-sqlite-benchmark_tests.gpr") and
                                    Project_File ("test_components/components-sqlite-sqlite_persistence_tests.gpr"))
                             ));

   end Sqlite;

   ------------------
   -- Strings_Edit --
   ------------------

   package Strings_Edit is

      function Project is new Catalogued_Project
        ("Simple Components (strings)");

      V_4_27 : constant Release :=
                              Project.Register
                                (Base_V_4_27
                                 .Extending
                                   (Properties         =>
                                      Project_File ("strings_edit.gpr"),

                                    Private_Properties =>
                                      Executable ("test_base64") and
                                      Executable ("test_strings_edit") and
                                      Executable ("test_string_streams") and

                                      Project_File ("test_strings_edit/strings_edit-test.gpr")));

   end Strings_Edit;

   package Tables is

      function Project is new Catalogued_Project
        ("Simple Components (tables)");

      V_4_27 : constant Release :=
                        Project.Register
                          (Base_V_4_27
                                             --                          .Replacing (Origin => Git (Repo, "19205e4981d72242daf72da7d59c5faf2b4c91fd"))
                           .Extending
                             (Properties         =>
                                Project_File ("tables.gpr"),

                              Private_Properties =>
                                Executable ("test_tables") and
                                Project_File ("test_tables/tables-test.gpr")));

   end Tables;

end Alire.Index.Simple_Components;
