```
// fortune program syntax example translated from python to iridium syntax
// from https://github.com/olkku45/fortune-python

include::std::Random;
include::std::File;
include::std::String;

allocator fn process_file(): []string {
    const quotes = File.open("quotes.txt", "r");
    defer File.close();

    const buffer: *u8 = alloc(quotes.bytesize());

    const without_newline: []string = {
        String.strip(quote, '\n', use=buffer) for quote in quotes
    };
    
    // optional warn.bound{} block for return, indicates 
    // memory is not freed here
    warn.bound{
        return without_newline;
    }
}

allocator fn get_quotes(file: []string): []string {
    // potential alternatives:
    // alloc(file.len() * file.max());
    // alloc(file.bytesize());
    const file_memory: *u32 = alloc(sum(i.len() for i in file));

    var quotes: []string = [""];
    const packer = String.packer(file_memory);

    const processed = process_file();

    for (line in processed) {
        const packed = packer.next(line);

        if (packed != '%') {
            if (quotes[-1]) {
                quotes[-1] += " ";
            }
            quotes[-1] += packed;
        }
        else {
            quotes.push("")
        }
    }

    free(file_memory);

    warn.bound{
        return quotes;
    }
}

fn get_random_quote(quotes: []string): string {
    return Random.choice(quotes);
}

fn main() {
    const processed = process_file();
    const quotes = get_quotes(processed);
    const random_quote = get_random_quote(quotes);
    println(random_quote);

    free(processed.buffer);
    free(processed);
    free(quotes);
}
```