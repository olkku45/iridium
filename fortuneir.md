```
// fortune program syntax example translated from python to iridium syntax
// from https://github.com/olkku45/fortune-python

include::std::Random;
include::std::File;
include::std::String;
include::std::Warn;

allocator fn process_file(): []string {
    const quotes = File::open("quotes.txt", "r");
    defer File::close(quotes);

    const buffer: *u8 = alloc(sizeof(quotes));

    const without_newline: []string = {
        String::strip(quote, '\n', use=buffer) for quote in quotes
    };
    
    // optional Warn::bound{} block for return, indicates 
    // memory is not freed here
    Warn::bound{ return without_newline };
}

allocator fn get_quotes(file: []string): []string {
    // potential alternatives:
    // alloc(file.len() * file.max());
    // alloc(file.bytesize());
    const file_memory: *u32 = alloc(sum(lengthof(i) for i in file));

    var quotes: []string = [""];
    const packer = String::packer(file_memory);

    const processed = process_file();

    for (line in processed) {
        const packed = packer::next(line);

        if (packed != '%') {
            if (quotes[-1]) {
                quotes[-1] += " ";
            }
            quotes[-1] += packed;
        }
        else {
            insert(quotes: "")
        }
    }

    free(file_memory);

    Warn::bound{ return quotes };
}

fn get_random_quote(quotes: []string): string {
    return Random::choice(quotes);
}

fn main() {
    const processed = process_file();
    const quotes = get_quotes(processed);
    const random_quote = get_random_quote(quotes);
    println(random_quote);

	// not sure about this: if 'buffer' is the keyword for an item in a struct, 
	// then this would work, but if it's a method-type property call, then I'd 
	// change it to something like buffer(processed)
    free(processed.buffer);
    free(processed);
    free(quotes);
}
```