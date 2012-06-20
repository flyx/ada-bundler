with Ada.Directories;
with Ada.Environment_Variables;

with Bundle.Configuration;

package body Bundle.OS is

   No_Valid_Dir_Found_Exception : exception;

   Home_Path : constant String := Ada.Environment_Variables.Value ("HOME");

   -- Select first fitting directory based on the value of an environment variable
   -- and a default list. If no valid directory is found in the variable / the
   -- default list, a path relative to the executable will be searched.
   function Dir_From_Env_Var (Dir_Var, Default : String)
							 return String is

	  function Is_Valid (Path : String) return Boolean is
		 use Ada.Directories;
	  begin
		 return Exists (Path) and then Kind (Path) = Directory;
	  end Is_Valid;

	  function Dir_From_List (List : String) return String is
		 Cur_Start : Integer := List'First;
		 Cur_End   : Integer := List'First;
	  begin
		 if List'Length = 0 then
			raise No_Valid_Dir_Found_Exception;
		 end if;

		 loop
			while Cur_End <= List'Last - 1 and then List (Cur_End) /= ':' loop
			   Cur_End := Cur_End + 1;
			end loop;
			declare
			   Cur_Dir : String := List (Cur_Start .. Cur_End);
			begin
			   if Is_Valid (Cur_Dir) then
				  return Cur_Dir;
			   end if;
			end;
			Cur_Start := Cur_End + 1;
			Cur_End   := Cur_Start;
			exit when Cur_End > List'Last;
		 end loop;
		 raise No_Valid_Dir_Found_Exception;
	  end Dir_From_List;

   begin
	  if Ada.Environment_Variables.Exists (Dir_Var) then
		 return Dir_From_List (Ada.Environment_Variables.Value (Dir_Var));
	  elsif Default'Length > 0 then
		 return Dir_From_List (Default);
	  else
		 raise No_Valid_Dir_Found_Exception;
	  end if;
   exception when No_Valid_Dir_Found_Exception =>
	  return Dir_From_List (Ada.Directories.Current_Directory);
   end Dir_From_Env_Var;

   function Configuration_Dir (Is_Generic, Append_Name : access Boolean) return String is
   begin
	  Is_Generic.all := True;
	  Append_Name.all := False;
	  return Ada.Directories.Current_Directory;
   end Configuration_Dir;

   function Data_Dir (Is_Generic, Append_Name : access Boolean) return String is
   begin
	  Is_Generic.all := True;
	  Append_Name.all := False;
	  return Ada.Directories.Current_Directory;
   end Data_Dir;

   function User_Configuration_Dir (Is_Generic, Append_Name : access Boolean) return String is
	  use Ada.Directories;
   begin
	  Is_Generic.all := False;
	  Append_Name.all := True;
	  return Dir_From_Env_Var ("XDG_CONFIG_HOME", Compose (Home_Path, ".config"));
   end User_Configuration_Dir;

   function User_Data_Dir (Is_Generic, Append_Name : access Boolean) return String is
	  use Ada.Directories;
   begin
	  Is_Generic.all := False;
	  Append_Name.all := True;
	  return Dir_From_Env_Var ("XDG_CONFIG_HOME", Compose (Compose (Home_Path, ".local"), "share"));
   end User_Data_Dir;

   function User_Cache_Dir (Is_Generic, Append_Name : access Boolean) return String is
	  use Ada.Directories;
   begin
	  Is_Generic.all := False;
	  Append_Name.all := True;
	  return Dir_From_Env_Var ("XDG_CACHE_HOME", Compose (Home_Path, ".cache"));
   end User_Cache_Dir;

   function User_Runtime_Dir (Is_Generic, Append_Name : access Boolean) return String is
	  use Ada.Directories;
	  Empty_String : constant String (1 .. 0) := (others => <>);
   begin
	  Is_Generic.all := False;
	  Append_Name.all := True;
	  return Dir_From_Env_Var ("XDG_RUNTIME_DIR", Empty_String);
   exception when No_Valid_Dir_Found_Exception =>
	  Is_Generic.all := False;
	  Append_Name.all := False;
	  return Ada.Directories.Current_Directory;
   end User_Runtime_Dir;

end Bundle.OS;