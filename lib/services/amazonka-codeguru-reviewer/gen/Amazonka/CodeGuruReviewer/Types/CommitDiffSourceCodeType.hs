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
-- Module      : Amazonka.CodeGuruReviewer.Types.CommitDiffSourceCodeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.CommitDiffSourceCodeType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A type of
-- <https://docs.aws.amazon.com/codeguru/latest/reviewer-api/API_SourceCodeType SourceCodeType>
-- that specifies the commit diff for a pull request on an associated
-- repository. The @SourceCommit@ and @DestinationCommit@ fields are
-- required to do a pull request code review.
--
-- /See:/ 'newCommitDiffSourceCodeType' smart constructor.
data CommitDiffSourceCodeType = CommitDiffSourceCodeType'
  { -- | The SHA of the source commit used to generate a commit diff. This field
    -- is required for a pull request code review.
    sourceCommit :: Prelude.Maybe Prelude.Text,
    -- | The SHA of the merge base of a commit.
    mergeBaseCommit :: Prelude.Maybe Prelude.Text,
    -- | The SHA of the destination commit used to generate a commit diff. This
    -- field is required for a pull request code review.
    destinationCommit :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CommitDiffSourceCodeType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceCommit', 'commitDiffSourceCodeType_sourceCommit' - The SHA of the source commit used to generate a commit diff. This field
-- is required for a pull request code review.
--
-- 'mergeBaseCommit', 'commitDiffSourceCodeType_mergeBaseCommit' - The SHA of the merge base of a commit.
--
-- 'destinationCommit', 'commitDiffSourceCodeType_destinationCommit' - The SHA of the destination commit used to generate a commit diff. This
-- field is required for a pull request code review.
newCommitDiffSourceCodeType ::
  CommitDiffSourceCodeType
newCommitDiffSourceCodeType =
  CommitDiffSourceCodeType'
    { sourceCommit =
        Prelude.Nothing,
      mergeBaseCommit = Prelude.Nothing,
      destinationCommit = Prelude.Nothing
    }

-- | The SHA of the source commit used to generate a commit diff. This field
-- is required for a pull request code review.
commitDiffSourceCodeType_sourceCommit :: Lens.Lens' CommitDiffSourceCodeType (Prelude.Maybe Prelude.Text)
commitDiffSourceCodeType_sourceCommit = Lens.lens (\CommitDiffSourceCodeType' {sourceCommit} -> sourceCommit) (\s@CommitDiffSourceCodeType' {} a -> s {sourceCommit = a} :: CommitDiffSourceCodeType)

-- | The SHA of the merge base of a commit.
commitDiffSourceCodeType_mergeBaseCommit :: Lens.Lens' CommitDiffSourceCodeType (Prelude.Maybe Prelude.Text)
commitDiffSourceCodeType_mergeBaseCommit = Lens.lens (\CommitDiffSourceCodeType' {mergeBaseCommit} -> mergeBaseCommit) (\s@CommitDiffSourceCodeType' {} a -> s {mergeBaseCommit = a} :: CommitDiffSourceCodeType)

-- | The SHA of the destination commit used to generate a commit diff. This
-- field is required for a pull request code review.
commitDiffSourceCodeType_destinationCommit :: Lens.Lens' CommitDiffSourceCodeType (Prelude.Maybe Prelude.Text)
commitDiffSourceCodeType_destinationCommit = Lens.lens (\CommitDiffSourceCodeType' {destinationCommit} -> destinationCommit) (\s@CommitDiffSourceCodeType' {} a -> s {destinationCommit = a} :: CommitDiffSourceCodeType)

instance Core.FromJSON CommitDiffSourceCodeType where
  parseJSON =
    Core.withObject
      "CommitDiffSourceCodeType"
      ( \x ->
          CommitDiffSourceCodeType'
            Prelude.<$> (x Core..:? "SourceCommit")
            Prelude.<*> (x Core..:? "MergeBaseCommit")
            Prelude.<*> (x Core..:? "DestinationCommit")
      )

instance Prelude.Hashable CommitDiffSourceCodeType where
  hashWithSalt salt' CommitDiffSourceCodeType' {..} =
    salt' `Prelude.hashWithSalt` destinationCommit
      `Prelude.hashWithSalt` mergeBaseCommit
      `Prelude.hashWithSalt` sourceCommit

instance Prelude.NFData CommitDiffSourceCodeType where
  rnf CommitDiffSourceCodeType' {..} =
    Prelude.rnf sourceCommit
      `Prelude.seq` Prelude.rnf destinationCommit
      `Prelude.seq` Prelude.rnf mergeBaseCommit

instance Core.ToJSON CommitDiffSourceCodeType where
  toJSON CommitDiffSourceCodeType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SourceCommit" Core..=) Prelude.<$> sourceCommit,
            ("MergeBaseCommit" Core..=)
              Prelude.<$> mergeBaseCommit,
            ("DestinationCommit" Core..=)
              Prelude.<$> destinationCommit
          ]
      )
