import React, { useMemo, useState, useCallback } from 'react';
import { createEditor } from 'slate';
import { Slate, Editable, withReact } from 'slate-react';

const Leaf = props => {
  return (
    <span
      {...props.attributes}
      style={{
        fontWeight: props.leaf.bold ? 'bold' : 'normal',
        fontStyle: props.leaf.italic ? 'italic' : 'normal',
      }}
    >
      {props.children}
    </span>
  );
};

const DefaultElement = props => {
  return <p {...props.attributes}>{props.children}</p>;
};

const QuoteElement = props => {
  return <blockquote {...props.attributes}>{props.children}</blockquote>;
};

const Qeditor = ({ body, changeFx }) => {
  const editor = useMemo(() => withReact(createEditor()), []);
  const [value, setValue] = useState(body);

  /* eslint-disable default-case, no-empty */
  const renderElement = useCallback(props => {
    console.log('renderElement props: ', props);
    switch (props.element.type) {
      case 'quote':
        return <QuoteElement {...props} />;
      default:
        return <DefaultElement {...props} />;
    }
  }, []);

  const renderLeaf = useCallback(props => {
    return <Leaf {...props} />;
  }, []);
  /* eslint-disable default-case, no-empty */
  return (
    <Slate
      editor={editor}
      value={value}
      onChange={change => {
        console.log(change);
        setValue(change);
        changeFx(change);
      }}
    >
      <Editable renderElement={renderElement} renderLeaf={renderLeaf} />
    </Slate>
  );
};

export default Qeditor;
