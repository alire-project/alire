with Ada.Exceptions;

procedure Last_Chance_Handler (E : Ada.Exceptions.Exception_Occurrence);
pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");
