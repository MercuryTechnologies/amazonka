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
-- Module      : Amazonka.Lambda.Types.ImageConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.ImageConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configuration values that override the container image Dockerfile
-- settings. See
-- <https://docs.aws.amazon.com/lambda/latest/dg/images-create.html#images-parms Container settings>.
--
-- /See:/ 'newImageConfig' smart constructor.
data ImageConfig = ImageConfig'
  { -- | Specifies parameters that you want to pass in with ENTRYPOINT.
    command :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the entry point to their application, which is typically the
    -- location of the runtime executable.
    entryPoint :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the working directory.
    workingDirectory :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'command', 'imageConfig_command' - Specifies parameters that you want to pass in with ENTRYPOINT.
--
-- 'entryPoint', 'imageConfig_entryPoint' - Specifies the entry point to their application, which is typically the
-- location of the runtime executable.
--
-- 'workingDirectory', 'imageConfig_workingDirectory' - Specifies the working directory.
newImageConfig ::
  ImageConfig
newImageConfig =
  ImageConfig'
    { command = Prelude.Nothing,
      entryPoint = Prelude.Nothing,
      workingDirectory = Prelude.Nothing
    }

-- | Specifies parameters that you want to pass in with ENTRYPOINT.
imageConfig_command :: Lens.Lens' ImageConfig (Prelude.Maybe [Prelude.Text])
imageConfig_command = Lens.lens (\ImageConfig' {command} -> command) (\s@ImageConfig' {} a -> s {command = a} :: ImageConfig) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the entry point to their application, which is typically the
-- location of the runtime executable.
imageConfig_entryPoint :: Lens.Lens' ImageConfig (Prelude.Maybe [Prelude.Text])
imageConfig_entryPoint = Lens.lens (\ImageConfig' {entryPoint} -> entryPoint) (\s@ImageConfig' {} a -> s {entryPoint = a} :: ImageConfig) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the working directory.
imageConfig_workingDirectory :: Lens.Lens' ImageConfig (Prelude.Maybe Prelude.Text)
imageConfig_workingDirectory = Lens.lens (\ImageConfig' {workingDirectory} -> workingDirectory) (\s@ImageConfig' {} a -> s {workingDirectory = a} :: ImageConfig)

instance Core.FromJSON ImageConfig where
  parseJSON =
    Core.withObject
      "ImageConfig"
      ( \x ->
          ImageConfig'
            Prelude.<$> (x Core..:? "Command" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "EntryPoint" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "WorkingDirectory")
      )

instance Prelude.Hashable ImageConfig where
  hashWithSalt salt' ImageConfig' {..} =
    salt' `Prelude.hashWithSalt` workingDirectory
      `Prelude.hashWithSalt` entryPoint
      `Prelude.hashWithSalt` command

instance Prelude.NFData ImageConfig where
  rnf ImageConfig' {..} =
    Prelude.rnf command
      `Prelude.seq` Prelude.rnf workingDirectory
      `Prelude.seq` Prelude.rnf entryPoint

instance Core.ToJSON ImageConfig where
  toJSON ImageConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Command" Core..=) Prelude.<$> command,
            ("EntryPoint" Core..=) Prelude.<$> entryPoint,
            ("WorkingDirectory" Core..=)
              Prelude.<$> workingDirectory
          ]
      )
