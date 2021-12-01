{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Comprehend.Types.SyntaxToken
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.SyntaxToken where

import Amazonka.Comprehend.Types.PartOfSpeechTag
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a work in the input text that was recognized and assigned a
-- part of speech. There is one syntax token record for each word in the
-- source text.
--
-- /See:/ 'newSyntaxToken' smart constructor.
data SyntaxToken = SyntaxToken'
  { -- | The zero-based offset from the beginning of the source text to the first
    -- character in the word.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | The word that was recognized in the source text.
    text :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a token.
    tokenId :: Prelude.Maybe Prelude.Int,
    -- | The zero-based offset from the beginning of the source text to the last
    -- character in the word.
    endOffset :: Prelude.Maybe Prelude.Int,
    -- | Provides the part of speech label and the confidence level that Amazon
    -- Comprehend has that the part of speech was correctly identified. For
    -- more information, see how-syntax.
    partOfSpeech :: Prelude.Maybe PartOfSpeechTag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SyntaxToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginOffset', 'syntaxToken_beginOffset' - The zero-based offset from the beginning of the source text to the first
-- character in the word.
--
-- 'text', 'syntaxToken_text' - The word that was recognized in the source text.
--
-- 'tokenId', 'syntaxToken_tokenId' - A unique identifier for a token.
--
-- 'endOffset', 'syntaxToken_endOffset' - The zero-based offset from the beginning of the source text to the last
-- character in the word.
--
-- 'partOfSpeech', 'syntaxToken_partOfSpeech' - Provides the part of speech label and the confidence level that Amazon
-- Comprehend has that the part of speech was correctly identified. For
-- more information, see how-syntax.
newSyntaxToken ::
  SyntaxToken
newSyntaxToken =
  SyntaxToken'
    { beginOffset = Prelude.Nothing,
      text = Prelude.Nothing,
      tokenId = Prelude.Nothing,
      endOffset = Prelude.Nothing,
      partOfSpeech = Prelude.Nothing
    }

-- | The zero-based offset from the beginning of the source text to the first
-- character in the word.
syntaxToken_beginOffset :: Lens.Lens' SyntaxToken (Prelude.Maybe Prelude.Int)
syntaxToken_beginOffset = Lens.lens (\SyntaxToken' {beginOffset} -> beginOffset) (\s@SyntaxToken' {} a -> s {beginOffset = a} :: SyntaxToken)

-- | The word that was recognized in the source text.
syntaxToken_text :: Lens.Lens' SyntaxToken (Prelude.Maybe Prelude.Text)
syntaxToken_text = Lens.lens (\SyntaxToken' {text} -> text) (\s@SyntaxToken' {} a -> s {text = a} :: SyntaxToken)

-- | A unique identifier for a token.
syntaxToken_tokenId :: Lens.Lens' SyntaxToken (Prelude.Maybe Prelude.Int)
syntaxToken_tokenId = Lens.lens (\SyntaxToken' {tokenId} -> tokenId) (\s@SyntaxToken' {} a -> s {tokenId = a} :: SyntaxToken)

-- | The zero-based offset from the beginning of the source text to the last
-- character in the word.
syntaxToken_endOffset :: Lens.Lens' SyntaxToken (Prelude.Maybe Prelude.Int)
syntaxToken_endOffset = Lens.lens (\SyntaxToken' {endOffset} -> endOffset) (\s@SyntaxToken' {} a -> s {endOffset = a} :: SyntaxToken)

-- | Provides the part of speech label and the confidence level that Amazon
-- Comprehend has that the part of speech was correctly identified. For
-- more information, see how-syntax.
syntaxToken_partOfSpeech :: Lens.Lens' SyntaxToken (Prelude.Maybe PartOfSpeechTag)
syntaxToken_partOfSpeech = Lens.lens (\SyntaxToken' {partOfSpeech} -> partOfSpeech) (\s@SyntaxToken' {} a -> s {partOfSpeech = a} :: SyntaxToken)

instance Core.FromJSON SyntaxToken where
  parseJSON =
    Core.withObject
      "SyntaxToken"
      ( \x ->
          SyntaxToken'
            Prelude.<$> (x Core..:? "BeginOffset")
            Prelude.<*> (x Core..:? "Text")
            Prelude.<*> (x Core..:? "TokenId")
            Prelude.<*> (x Core..:? "EndOffset")
            Prelude.<*> (x Core..:? "PartOfSpeech")
      )

instance Prelude.Hashable SyntaxToken where
  hashWithSalt salt' SyntaxToken' {..} =
    salt' `Prelude.hashWithSalt` partOfSpeech
      `Prelude.hashWithSalt` endOffset
      `Prelude.hashWithSalt` tokenId
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` beginOffset

instance Prelude.NFData SyntaxToken where
  rnf SyntaxToken' {..} =
    Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf partOfSpeech
      `Prelude.seq` Prelude.rnf endOffset
      `Prelude.seq` Prelude.rnf tokenId
      `Prelude.seq` Prelude.rnf text
