# Elixir Koans

The Elixir Koans walk you along the path to enlightenment in order to learn Elixir.
The goal is to learn the Elixir language, syntax, structure, and some common
functions and libraries. We also teach you culture by basing the koans on tests.
Testing is not just something we pay lip service to, but something we
live.  Testing is essential in your quest to learn and do great things in Elixir.

## The Structure

The koans are broken out into areas by file, hashes are covered in `about_hashes.ex`,
modules are introduced in `about_modules.ex`, *etc*.  They are presented in
order in the `path_to_enlightenment.ex` file.

Each koan builds up your knowledge of Ruby and builds upon itself. It will stop at
the first place you need to correct.

Some koans simply need to have the correct answer substituted for an incorrect one.
Some, however, require you to supply your own answer.  If you see the method `__` (a
double underscore) listed, it is a hint to you to supply your own code in order to
make it work correctly.

## Installing Elixir

If you do not have Elixir setup, please visit <http://elixir-lang.org/getting_started/1.html> for
operating system specific instructions.  In order to run the koans, `elixir` is all you need. 
To check your installations simply type:

*nix platforms from any terminal window:

   [~] $ elixir --version

Windows from the command prompt (`cmd.exe`)

   c:\\> elixir --version

Any response for Elixir with a version number is fine.

## Generating the Koans

A fresh checkout will not include the koans, you will need to generate
them.

    [elixir_koans] $ mix gen                      # generates the koans directory

If you need to regenerate the koans, thus wiping your current `koans`,

    [elixir_koans] $ mix regen                    # regenerates the koans directory, wiping the original

## The Path To Enlightenment

You can run the tests through `mix` or by calling the file itself (`mix` is the
recommended way to run them as we might build more functionality into this task).

*nix platforms, from the `elixir_koans` directory

    [elixir_koans] $ mix                              # runs the default target :walk_the_path
    [elixir_koans] $ elixir path_to_enlightenment.ex  # simply call the file directly

Windows is the same thing

    c:\> elixir_koans\mix                              # runs the default target :walk_the_path
    c:\> elixir_koans\elixir path_to_enlightenment.ex  # simply call the file directly

### Red, Green, Refactor

In test-driven development the mantra has always been *red, green, refactor*.
Write a failing test and run it (*red*), make the test pass (*green*),
then look at the code and consider if you can make it any better (*refactor*).

While walking the path to Elixir enlightenment you will need to run the koan and
see it fail (*red*), make the test pass (*green*), then take a moment
and reflect upon the test to see what it is teaching you and improve the code to
better communicate its intent (*refactor*).

The very first time you run the koans you will see the following output:

    [ elixir_koans ] $ mix
    (in /Users/person/dev/elixir_koans)
    /usr/bin/elixir1.8 path_to_enlightenment.ex

    AboutAsserts#test_assert_truth has damaged your karma.

    The Master says:
    You have not yet reached enlightenment.

    The answers you seek...
    <false> is not true.

    Please meditate on the following code:
    ./about_asserts.ex:10:in `test_assert_truth'
    path_to_enlightenment.ex:38:in `each_with_index'
    path_to_enlightenment.ex:38

    mountains are merely mountains
    your path thus far [X_________________________________________________] 0/280

You have come to your first stage. Notice it is telling you where to look for
the first solution:

    Please meditate on the following code:
    ./about_asserts.ex:10:in `test_assert_truth'
    path_to_enlightenment.ex:38:in `each_with_index'
    path_to_enlightenment.ex:38

Open the `about_asserts.ex` file and look at the first test:

    # We shall contemplate truth by testing reality, via asserts.
    def test_assert_truth
      assert false                # This should be true
    end

Change the `false` to `true` and re-run the test.  After you are
done, think about what you are learning.  In this case, ignore everything except
the method name (`test_assert_truth`) and the parts inside the method (everything
before the `end`).

In this case the goal is for you to see that if you pass a value to the `assert`
method, it will either ensure it is `true` and continue on, or fail if
the statement is `false`.

### Running the Koans automatically

*This section is optional.*

Normally the path to enlightenment looks like this:

    cd elixir_koans
    mix
    # edit
    mix
    # edit
    mix
    # etc

If you prefer, you can keep the koans running in the background so that after you
make a change in your editor, the koans will immediately run again. This will
hopefully keep your focus on learning Elixir instead of on the command line.

Install the Elixir gem (library) called `watchr` and then ask it to
"watch" the koans for changes:

    cd elixir_koans
    mix
    # decide to run mix automatically from now on as you edit
    gem install watchr
    watchr ./koans/koans.watchr

## Inspiration

This began as a port of the [Ruby Koans](https://github.com/neo/ruby_koans) which 
proved to be a great way to learn the essentials of the Ruby language.  Many thanks 
to Jim Weirich and all who contributed to that project.

## Other Resources

<table>
  <tr>
    <td>The Elixir Language</td>
    <td>
      <a href="http://elixir-lang.org">http://elixir-lang.org</a>
    </td>
  </tr>
</table>


# Other stuff

<table>
  <tr>
    <td>Author</td>
    <td>Bryan Ash</td>
  </tr>
  <tr>
    <td>Issue Tracker</td>
    <td>TBD</td>
  </tr>
</table>

# License

![Creative Commons, Attribution-NonCommercial-ShareAlike, Version 3.0](http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png)

ElixirKoans is released under a Creative Commons,
Attribution-NonCommercial-ShareAlike, Version 3.0
<http://creativecommons.org/licenses/by-nc-sa/3.0/> License.
