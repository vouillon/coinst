(* Co-installability tools
 * http://coinst.irill.org/
 * Copyright (C) 2005-2011 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * These programs are free software; you can redistribute them and/or
 * modify them under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

type st =
  { time : float;
    active : bool;
    channel : in_channel;
    length : int;
    mutable count : int;
    mutable percent : float }

let start_parsing active ch =
  { time = Unix.gettimeofday ();
    active = active; channel = ch;
    length = begin try in_channel_length ch with Sys_error _ -> 0 end;
    count = 0; percent = 0. }

let parsing_tick st =
  st.count <- st.count + 1;
  if st.active then begin
    if st.length > 0 then begin
      let p = pos_in st.channel in
      let pc = float p *. 100. /. float st.length in
      if pc >= st.percent then begin
        Util.set_msg (Format.sprintf
                        "Parsing package file...  %3.f%%  %6d packages"
                        pc st.count);
        st.percent <- pc +. 1.
      end
    end else if st.count mod 10 = 0 then
      Util.set_msg
        (Format.sprintf "Parsing package file...  %6d packages" st.count)
  end

let stop_parsing st =
  Util.set_msg "";
  Format.eprintf "Parsing package file...  %.1f seconds  %6d packages@."
    (Unix.gettimeofday () -. st.time) st.count

(****)

type st' =
  { time : float;
    active : bool;
    num : int;
    step : int;
    mutable count : int }

let start_generate active num =
  if active then Util.set_msg "Generating constraints...";
  { time = Unix.gettimeofday ();
    active = active; num = num; step = max 1 (num / 100); count = 0 }

let generate_next st =
  st.count <- st.count + 1;
  if st.active && st.count mod st.step = 0 then
    Util.set_msg (Format.sprintf "Generating constraints...  %3.f%%"
                    (float st.count *. 100. /. float st.num))

let stop_generate st =
  Util.set_msg "";
  Format.eprintf "Generating constraints...  %.1f seconds@."
                    (Unix.gettimeofday () -. st.time);
