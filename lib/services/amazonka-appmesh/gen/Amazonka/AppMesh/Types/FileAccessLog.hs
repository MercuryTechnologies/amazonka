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
-- Module      : Amazonka.AppMesh.Types.FileAccessLog
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.FileAccessLog where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents an access log file.
--
-- /See:/ 'newFileAccessLog' smart constructor.
data FileAccessLog = FileAccessLog'
  { -- | The file path to write access logs to. You can use @\/dev\/stdout@ to
    -- send access logs to standard out and configure your Envoy container to
    -- use a log driver, such as @awslogs@, to export the access logs to a log
    -- storage service such as Amazon CloudWatch Logs. You can also specify a
    -- path in the Envoy container\'s file system to write the files to disk.
    --
    -- The Envoy process must have write permissions to the path that you
    -- specify here. Otherwise, Envoy fails to bootstrap properly.
    path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileAccessLog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'fileAccessLog_path' - The file path to write access logs to. You can use @\/dev\/stdout@ to
-- send access logs to standard out and configure your Envoy container to
-- use a log driver, such as @awslogs@, to export the access logs to a log
-- storage service such as Amazon CloudWatch Logs. You can also specify a
-- path in the Envoy container\'s file system to write the files to disk.
--
-- The Envoy process must have write permissions to the path that you
-- specify here. Otherwise, Envoy fails to bootstrap properly.
newFileAccessLog ::
  -- | 'path'
  Prelude.Text ->
  FileAccessLog
newFileAccessLog pPath_ =
  FileAccessLog' {path = pPath_}

-- | The file path to write access logs to. You can use @\/dev\/stdout@ to
-- send access logs to standard out and configure your Envoy container to
-- use a log driver, such as @awslogs@, to export the access logs to a log
-- storage service such as Amazon CloudWatch Logs. You can also specify a
-- path in the Envoy container\'s file system to write the files to disk.
--
-- The Envoy process must have write permissions to the path that you
-- specify here. Otherwise, Envoy fails to bootstrap properly.
fileAccessLog_path :: Lens.Lens' FileAccessLog Prelude.Text
fileAccessLog_path = Lens.lens (\FileAccessLog' {path} -> path) (\s@FileAccessLog' {} a -> s {path = a} :: FileAccessLog)

instance Core.FromJSON FileAccessLog where
  parseJSON =
    Core.withObject
      "FileAccessLog"
      ( \x ->
          FileAccessLog' Prelude.<$> (x Core..: "path")
      )

instance Prelude.Hashable FileAccessLog where
  hashWithSalt salt' FileAccessLog' {..} =
    salt' `Prelude.hashWithSalt` path

instance Prelude.NFData FileAccessLog where
  rnf FileAccessLog' {..} = Prelude.rnf path

instance Core.ToJSON FileAccessLog where
  toJSON FileAccessLog' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("path" Core..= path)]
      )
