================================================================================
  Title
================================================================================

Here are some words.
More words from the same paragraph.::

    (defmethod process-node :CODE_BLOCK [[_ & lines]]
      {:type :code
       :lines (mapv (fn [[_ [_ white-space] [_ & content]]]
                      {:white-space white-space
                       :content (apply str content)})
                    lines)})

This is how you take care of Hawthorne. First, feed him

First Section
================================================================================

First Subsection
--------------------------------------------------------------------------------

The words of a section.

First Sub-subsection
````````````````````````````````````````````````````````````````````````````````

Need something here?

Second SubSection: Bathing Hawthie
--------------------------------------------------------------------------------

First, fill the tub

Second Section: etc
================================================================================

More words I guess(?)

- This is the first bullet list item.  The blank line above the
  first list item is required; blank lines between list items
  (such as below this paragraph) are optional.

- This is the first paragraph in the second item in the list.

  This is the second paragraph in the second item in the list.
  The blank line above this paragraph is required.  The left edge
  of this paragraph lines up with the paragraph above, both
  indented relative to the bullet.

  - This is a sublist.  The bullet lines up with the left edge of
    the text blocks above.  A sublist is a new list so requires a
    blank line above and below.

- This is the third item of the main list.

This paragraph is not part of the list.
