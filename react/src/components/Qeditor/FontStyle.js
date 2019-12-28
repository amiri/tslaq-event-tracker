import React from 'react';
import { Editor, Text, Transforms } from 'slate';
import classnames from 'classnames';
import { Button } from 'antd';

const iconName = {
  bold: 'bold',
  italic: 'italic',
  strikethrough: 'strikethrough',
  underline: 'underline',
};

export const isStyle = ({ editor, fontStyle }) => {
  const [match] = Editor.nodes(editor, {
    match: n => n[fontStyle] === true,
    universal: true,
  });
  return !!match;
};

export const toggleStyle = ({ editor, fontStyle }) => {
  const set = isStyle({ editor, fontStyle });
  Transforms.setNodes(
    editor,
    { [fontStyle]: set ? null : true },
    { match: n => Text.isText(n), split: true },
  );
};

export const StyleButton = ({ editor, fontStyle }) => {
  return (
    <Button
      size='small'
      icon={iconName[fontStyle]}
      onMouseDown={e => {
        e.preventDefault();
        toggleStyle({ editor, fontStyle });
      }}
      className={classnames({
        active: isStyle({ editor, fontStyle }),
      })}
    />
  );
};
