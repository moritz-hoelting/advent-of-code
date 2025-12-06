unit module day06;

sub part1_process_column(@column) {
    my @mutable = @column.Array;
    my $last = @mutable.pop;

    if $last eq '*' {
        return [×] @mutable.map( *.Int );
    } elsif $last eq '+' {
        return @mutable.map( *.Int ).sum;
    }

    die "Unknown operation: $last";
}

sub part1($input) is export {
    my @rows = $input.lines.map( *.words ).Array;
    my @columns = [Z] @rows;
    return @columns.map( -> @row { part1_process_column(@row) } ).sum;
}

sub part2($input) is export {
    my @lines = $input.lines;

    my $op-line = @lines.pop;
    my @ops;
    for $op-line.comb.kv -> $i, $char {
        if $char eq '*' || $char eq '+' {
            @ops.push( ($char, $i) );
        }
    }

    my @columns;
    for @ops.kv -> $i, ($op, $pos) {
        my $next_pos = $i + 1 < @ops.elems ?? @ops[$i + 1][1] !! $op-line.chars;
        my @nums = (1..($next_pos-$pos)).map({ [] });
        loop (my $j = $pos; $j < $next_pos; $j++) {
            for @lines -> $line {
                my $substr = $line.substr($j, 1);
                if $substr ~~ /\d/ {
                    @nums[$j - $pos].push($substr);
                }
            }
        }

        my @nums_processed = @nums.grep(*.elems).map({ .join('').Int });

        @columns.push( { op => $op, nums => @nums_processed } );
    }

    return @columns.map({ if $_<op> eq '*' { [×] @($_<nums>) } else { $_<nums>.sum } }).sum;
}