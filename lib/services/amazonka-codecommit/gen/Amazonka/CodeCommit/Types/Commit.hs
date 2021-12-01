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
-- Module      : Amazonka.CodeCommit.Types.Commit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.Commit where

import Amazonka.CodeCommit.Types.UserInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a specific commit.
--
-- /See:/ 'newCommit' smart constructor.
data Commit = Commit'
  { -- | The full SHA ID of the specified commit.
    commitId :: Prelude.Maybe Prelude.Text,
    -- | Information about the person who committed the specified commit, also
    -- known as the committer. Information includes the date in timestamp
    -- format with GMT offset, the name of the committer, and the email address
    -- for the committer, as configured in Git.
    --
    -- For more information about the difference between an author and a
    -- committer in Git, see
    -- <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro
    -- Git by Scott Chacon and Ben Straub.
    committer :: Prelude.Maybe UserInfo,
    -- | Tree information for the specified commit.
    treeId :: Prelude.Maybe Prelude.Text,
    -- | Any other data associated with the specified commit.
    additionalData :: Prelude.Maybe Prelude.Text,
    -- | A list of parent commits for the specified commit. Each parent commit ID
    -- is the full commit ID.
    parents :: Prelude.Maybe [Prelude.Text],
    -- | Information about the author of the specified commit. Information
    -- includes the date in timestamp format with GMT offset, the name of the
    -- author, and the email address for the author, as configured in Git.
    author :: Prelude.Maybe UserInfo,
    -- | The commit message associated with the specified commit.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Commit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitId', 'commit_commitId' - The full SHA ID of the specified commit.
--
-- 'committer', 'commit_committer' - Information about the person who committed the specified commit, also
-- known as the committer. Information includes the date in timestamp
-- format with GMT offset, the name of the committer, and the email address
-- for the committer, as configured in Git.
--
-- For more information about the difference between an author and a
-- committer in Git, see
-- <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro
-- Git by Scott Chacon and Ben Straub.
--
-- 'treeId', 'commit_treeId' - Tree information for the specified commit.
--
-- 'additionalData', 'commit_additionalData' - Any other data associated with the specified commit.
--
-- 'parents', 'commit_parents' - A list of parent commits for the specified commit. Each parent commit ID
-- is the full commit ID.
--
-- 'author', 'commit_author' - Information about the author of the specified commit. Information
-- includes the date in timestamp format with GMT offset, the name of the
-- author, and the email address for the author, as configured in Git.
--
-- 'message', 'commit_message' - The commit message associated with the specified commit.
newCommit ::
  Commit
newCommit =
  Commit'
    { commitId = Prelude.Nothing,
      committer = Prelude.Nothing,
      treeId = Prelude.Nothing,
      additionalData = Prelude.Nothing,
      parents = Prelude.Nothing,
      author = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The full SHA ID of the specified commit.
commit_commitId :: Lens.Lens' Commit (Prelude.Maybe Prelude.Text)
commit_commitId = Lens.lens (\Commit' {commitId} -> commitId) (\s@Commit' {} a -> s {commitId = a} :: Commit)

-- | Information about the person who committed the specified commit, also
-- known as the committer. Information includes the date in timestamp
-- format with GMT offset, the name of the committer, and the email address
-- for the committer, as configured in Git.
--
-- For more information about the difference between an author and a
-- committer in Git, see
-- <http://git-scm.com/book/ch2-3.html Viewing the Commit History> in Pro
-- Git by Scott Chacon and Ben Straub.
commit_committer :: Lens.Lens' Commit (Prelude.Maybe UserInfo)
commit_committer = Lens.lens (\Commit' {committer} -> committer) (\s@Commit' {} a -> s {committer = a} :: Commit)

-- | Tree information for the specified commit.
commit_treeId :: Lens.Lens' Commit (Prelude.Maybe Prelude.Text)
commit_treeId = Lens.lens (\Commit' {treeId} -> treeId) (\s@Commit' {} a -> s {treeId = a} :: Commit)

-- | Any other data associated with the specified commit.
commit_additionalData :: Lens.Lens' Commit (Prelude.Maybe Prelude.Text)
commit_additionalData = Lens.lens (\Commit' {additionalData} -> additionalData) (\s@Commit' {} a -> s {additionalData = a} :: Commit)

-- | A list of parent commits for the specified commit. Each parent commit ID
-- is the full commit ID.
commit_parents :: Lens.Lens' Commit (Prelude.Maybe [Prelude.Text])
commit_parents = Lens.lens (\Commit' {parents} -> parents) (\s@Commit' {} a -> s {parents = a} :: Commit) Prelude.. Lens.mapping Lens.coerced

-- | Information about the author of the specified commit. Information
-- includes the date in timestamp format with GMT offset, the name of the
-- author, and the email address for the author, as configured in Git.
commit_author :: Lens.Lens' Commit (Prelude.Maybe UserInfo)
commit_author = Lens.lens (\Commit' {author} -> author) (\s@Commit' {} a -> s {author = a} :: Commit)

-- | The commit message associated with the specified commit.
commit_message :: Lens.Lens' Commit (Prelude.Maybe Prelude.Text)
commit_message = Lens.lens (\Commit' {message} -> message) (\s@Commit' {} a -> s {message = a} :: Commit)

instance Core.FromJSON Commit where
  parseJSON =
    Core.withObject
      "Commit"
      ( \x ->
          Commit'
            Prelude.<$> (x Core..:? "commitId")
            Prelude.<*> (x Core..:? "committer")
            Prelude.<*> (x Core..:? "treeId")
            Prelude.<*> (x Core..:? "additionalData")
            Prelude.<*> (x Core..:? "parents" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "author")
            Prelude.<*> (x Core..:? "message")
      )

instance Prelude.Hashable Commit where
  hashWithSalt salt' Commit' {..} =
    salt' `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` author
      `Prelude.hashWithSalt` parents
      `Prelude.hashWithSalt` additionalData
      `Prelude.hashWithSalt` treeId
      `Prelude.hashWithSalt` committer
      `Prelude.hashWithSalt` commitId

instance Prelude.NFData Commit where
  rnf Commit' {..} =
    Prelude.rnf commitId
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf author
      `Prelude.seq` Prelude.rnf parents
      `Prelude.seq` Prelude.rnf additionalData
      `Prelude.seq` Prelude.rnf treeId
      `Prelude.seq` Prelude.rnf committer
