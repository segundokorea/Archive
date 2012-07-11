#!/bin/bash
# del
# "Safe" version of rm
# Updated 2010-01-13 by Sean Clemmer
#
# If I learned one thing writing this script it's as follows:
#   When in doubt, quote.
# At the very least, this thing can handle spaces

usage="USAGE: del [-vler] file*"
# -v, --view    View the trash logfile
# -l, --list    List contents of trash
# -e, --empty   Empty the trash (forever)
# -r, --recover Start recovery mode
# file          File to delete or recover
# Note: -e, --empty and -r, --recover only work separately
#       I won't stop you, but beware!
# Note: Flags will execute in order given
#       This is a feature, not a bug


owd="$( pwd )"                # Original working directory
# Turns out Mac OS X uses ~/.Trash, so...don't do that
trash="${HOME}/.trash.del"    # Trash directory
logfile="${HOME}/.trash.log"  # Path to logfile
logfile_format="recover.${$}" # Don't change this
tempfile="/tmp/recover.${$}"  # Remember to rm


# Make sure we have a trash directory
if [[ ! -d "${trash}" ]]
then
  mkdir "${trash}"
fi

# Make sure we have a logfile
if [[ ! -f "${logfile}" ]]
then
  touch "${logfile}"
fi


# -v, --view
# Helpful for debugging; doesn't do formatting
# Should be like -l, --list except more verbose
function view_log {
  cat "${logfile}"
}


# -l, --list
function list_trash {
  ls "${trash}" | sed 's/\.recover\.[0-9]*$//g'
}


# -e, --empty
function empty_trash {
  rm -rf "${trash}"
  mkdir -p "${trash}"
  rm -f "${logfile}"
  touch "${logfile}"
  echo "Emptied trash"
}


# Handle command-line arguments
# Work as flags, so each gets called only once
# Print usage if nothing is passed
if [[ "${#@}" -eq 0 ]]
then
  echo "${usage}"
  exit 0
fi

declare -a files
i=0
for arg in "${@}"
do
  if [[ "${arg}" == '-v' || "${arg}" == '--view' ]]
  then
    view_log
  elif [[ "${arg}" == '-l' || "${arg}" == '--list' ]]
  then
    list_trash
  elif [[ "${arg}" == '-e' || "${arg}" == '--empty' ]]
  then
    empty_trash
  elif [[ "${arg}" == '-r' || "${arg}" == '--recover' ]]
  then
    do_recovery_mode=true
  else
    files[$i]="${arg}"
    i=$(( i + 1 ))
  fi
done


# -r, --recover
# Heart of the recovery operation: read_choice(), update_information_for_match(), recover_match()
function read_choice {
  # Read isn't too bright; hope the user is
  read -e -p "Choose from above: " choice
  if [[ ${choice} -ge 0 && ${choice} -lt $( echo "${matches}" | wc -l ) ]]
  then
    IFS=$( echo -en "\n\b" ) # Change the for loop separator to newline
      matches=( $( echo "${matches}" ) )
      match=${matches[$choice]}
    IFS=${saved_IFS}
  else
    echo "WARNING: '${choice}' is not a valid choice"
    read_choice
  fi
}

# Make sure you've got $match when you use this or any other function
function update_information_for_match {
  base="$( echo "${match}" | awk -F:: '{ print $1 }' )"
  dir="$( echo "${match}" | awk -F:: '{ print $2 }' )"
  name="$( echo "${match}" | awk -F:: '{ print $3 }' )"
  when="$( echo "${match}" | awk -F:: '{ print $4 }' )"
}

function recover_match {
  # Remove entry from the log, move the actual file
  # Appened .recovered so we don't overwrite anything
  # (Yeah, this is pure laziness on my part...)
  update_information_for_match

  # Mission critical: I'll trap to be safe
  trap "echo 'INTERRUPTED: Emptying trash...'; empty_trash;" INT TERM EXIT
    touch "${tempfile}"
    sed "$( echo "/::${name}::/d" )" "${logfile}" > "${tempfile}"
    mv "${tempfile}" "${logfile}"
    mv "${trash}/${name}" "${dir}/${base}.recovered"
  trap - INT TERM EXIT

  echo "Recovered ${base} (${when}) to ${dir}"
}


for file in "${files}"
do
  cd "${owd}"

  # -r, --recover
  if [[ ${do_recovery_mode} ]]
  then
    i=0
    matches=$( cat ${logfile} | sed -n "$( echo "/^${file}::/p" )" )
    if [[ "${matches}" ]]
    then
      if [[ $( echo "${matches}" | wc -l ) -gt 1 ]]
      then
        # Multiple matches, let the user decide
        echo "Possible maches for '${file}':"
        saved_IFS=${IFS}
        IFS=$( echo -en "\n\b" )
          for match in ${matches}
          do
            update_information_for_match
            echo "${i} - ${base} (${when}) in ${dir}"
            i=$(( i + 1 ))
          done
        IFS=${saved_IFS}
        read_choice
        recover_match
      else
        # No ambiguous occurrences of $file in the $logfile
        # Skip all that other stuff and just recover it
        match="${matches}"
        recover_match
      fi
    else
      echo "ERROR: No matches for '${file}' found"
      suggestions=$( cat ${logfile} | sed -n "$( echo "/^${file}/p" )" | awk -F:: '{ print $1 }' | sort -u )
      if [[ ! "${suggestions}" == "" ]]
      then
        echo "Maybe you meant '$( echo ${suggestions} | head -n 1 )'?"
      fi
      echo "${usage}"
      exit 1
    fi

  # Handle some weird spaces behavior...
  elif [[ "${file}" == "" ]]
  then
    break;

  # Not recovery mode, so just trash the file
  else
    if [[ -f "${file}" ]]
    then
      cd "$( dirname "${file}" )"
      dir="$( pwd )"
      base="$( basename "${file}" )"

      # Beauty of this format means you can have two files with the same name from the same
      # directory and restore them independently (make sure you move your *.recovered file)
      mv "${file}" "${trash}/${file}.${logfile_format}"
      echo "${base}::${dir}::${file}.${logfile_format}::$( date )" >> "${logfile}"
    else
      echo "ERROR: '${file}' is not a valid file"
      echo "${usage}"
      exit 1
    fi
  fi
done

exit 0
