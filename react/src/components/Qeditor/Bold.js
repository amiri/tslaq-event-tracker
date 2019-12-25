import { Editor, Text, Transforms } from 'slate';

export const isBold = editor => {
  const [match] = Editor.nodes(editor, {
    match: n => n.bold === true,
    universal: true,
  });
  return !!match;
};

export const toggleBold = editor => {
  const isBold = isBold(editor);
  Transforms.setNodes(
    editor,
    { bold: isBold ? null : true },
    { match: n => Text.isText(n), split: true },
  );
};
