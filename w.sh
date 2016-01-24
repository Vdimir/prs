
while true; do
    change=$(inotifywait -e close_write,moved_to,create src/)
    clear
    echo $change changed
    cargo run

done
