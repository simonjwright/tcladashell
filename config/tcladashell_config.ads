--  Configuration for tcladashell generated by Alire
pragma Restrictions (No_Elaboration_Code);
pragma Style_Checks (Off);

package Tcladashell_Config is
   pragma Pure;

   Crate_Version : constant String := "8.7.0-dev";
   Crate_Name : constant String := "tcladashell";

   Alire_Host_OS : constant String := "macos";

   Alire_Host_Arch : constant String := "x86_64";

   Alire_Host_Distro : constant String := "distro_unknown";

   type Build_Profile_Kind is (release, validation, development);
   Build_Profile : constant Build_Profile_Kind := development;

end Tcladashell_Config;
