package fr.esgi.al.funprog.model

enum Action:
  case Incrementer, Decrementer, Switch

object Action:
  def fromString(s: String): Option[Action] = s.trim match
    case "+" => Some(Incrementer)
    case "-" => Some(Decrementer)
    case "%" => Some(Switch)
    case _   => None
