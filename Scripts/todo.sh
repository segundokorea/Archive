#!/bin/bash

# Pseudo-globals
todo_file=$HOME/.todo
temp_file=/tmp/todo-$$
temp_file2=/tmp/todo--$$

# Make sure we've got a todo list
if [[ ! -e ${todo_file} ]]
then
  echo "List created."
  touch ${todo_file}
fi

# Without arguments, print items
if [[ ${#} -eq 0 ]]
then
  if [[ $( cat ${todo_file} | wc -l ) -gt 0 ]]
  then
    cat -n ${todo_file}
    exit 0
  else
    echo "List empty."
    exit 0
  fi
fi

# With -d argument, delete items
if [ "${1}" == "-d" ] # Trouble with extended test construction
then
  # Going to need these later
  touch ${temp_file}
  touch ${temp_file2}
  deletions=0
  argv=${@:2} # Just the facts ma'am

  # Process each argument to -d
  # Remember: We may not be passed nice stuff
  for arg in ${argv}
  do
    # Check to see if argument is actually an item
    # That is, is argument between zero and number of todo items?
    if [ `cat ${todo_file} | wc -l` -ge ${arg} -a ${arg} -gt 0 ]
    then
      # Can't delete willy-nilly or indexing gets off
      # We'll have to store what we're supposed to delete and remove it later
      echo "$( sed -n ${arg}p ${todo_file} )" >> ${temp_file}
      deletions=$(( $deletions + 1 ))
    fi
  done

  # Diff turns out to be a pretty good solution (I think)
  # Keep only the stuff you want and put it back into the todo
  diff ${todo_file} ${temp_file} | sed -n 's/^< //p' > ${temp_file2}
  cp ${temp_file2} ${todo_file}

  # Winding down...
  echo "${deletions} items deleted."
  if [[ $( cat ${todo_file} | wc -l ) -eq 0 ]]
  then
    echo "List empty."
  fi
  rm ${temp_file}
  rm ${temp_file2}
  exit 0
fi

# With n arguments, add items
for arg in "${@}"
do
  echo ${arg} >> ${todo_file}
done
echo "${#} items added."
exit 0
