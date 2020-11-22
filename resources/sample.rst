Document
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

Section
--------------------------------------------------------------------------------

The words of a section.


Bathing Hawthie
--------------------------------------------------------------------------------

First, fill the tub
