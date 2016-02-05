
while true; do
    clear
    if [ $1 = "test" ]
    then
        cargo test
    fi

    if [ $? = 0 ]
    then
        tput bold;tput setaf 2;
        echo "****************"
        echo "****SUCCESS*****"
        echo "****************"
    else
        tput bold; tput setaf 1;
        echo "****************"
        echo "******FAIL******"
        echo "****************"
    fi
    tput sgr0;
    change=$(inotifywait -e close_write,moved_to,create */*.rs)
    echo $change changed
done
