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
-- Module      : Amazonka.SageMaker.Types.CompilationJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.CompilationJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CompilationJobStatus
import Amazonka.SageMaker.Types.TargetDevice
import Amazonka.SageMaker.Types.TargetPlatformAccelerator
import Amazonka.SageMaker.Types.TargetPlatformArch
import Amazonka.SageMaker.Types.TargetPlatformOs

-- | A summary of a model compilation job.
--
-- /See:/ 'newCompilationJobSummary' smart constructor.
data CompilationJobSummary = CompilationJobSummary'
  { -- | The time when the model compilation job started.
    compilationStartTime :: Prelude.Maybe Core.POSIX,
    -- | The type of accelerator that the model will run on after the compilation
    -- job has completed.
    compilationTargetPlatformAccelerator :: Prelude.Maybe TargetPlatformAccelerator,
    -- | The type of device that the model will run on after the compilation job
    -- has completed.
    compilationTargetDevice :: Prelude.Maybe TargetDevice,
    -- | The time when the model compilation job was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The type of architecture that the model will run on after the
    -- compilation job has completed.
    compilationTargetPlatformArch :: Prelude.Maybe TargetPlatformArch,
    -- | The time when the model compilation job completed.
    compilationEndTime :: Prelude.Maybe Core.POSIX,
    -- | The type of OS that the model will run on after the compilation job has
    -- completed.
    compilationTargetPlatformOs :: Prelude.Maybe TargetPlatformOs,
    -- | The name of the model compilation job that you want a summary for.
    compilationJobName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model compilation job.
    compilationJobArn :: Prelude.Text,
    -- | The time when the model compilation job was created.
    creationTime :: Core.POSIX,
    -- | The status of the model compilation job.
    compilationJobStatus :: CompilationJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompilationJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compilationStartTime', 'compilationJobSummary_compilationStartTime' - The time when the model compilation job started.
--
-- 'compilationTargetPlatformAccelerator', 'compilationJobSummary_compilationTargetPlatformAccelerator' - The type of accelerator that the model will run on after the compilation
-- job has completed.
--
-- 'compilationTargetDevice', 'compilationJobSummary_compilationTargetDevice' - The type of device that the model will run on after the compilation job
-- has completed.
--
-- 'lastModifiedTime', 'compilationJobSummary_lastModifiedTime' - The time when the model compilation job was last modified.
--
-- 'compilationTargetPlatformArch', 'compilationJobSummary_compilationTargetPlatformArch' - The type of architecture that the model will run on after the
-- compilation job has completed.
--
-- 'compilationEndTime', 'compilationJobSummary_compilationEndTime' - The time when the model compilation job completed.
--
-- 'compilationTargetPlatformOs', 'compilationJobSummary_compilationTargetPlatformOs' - The type of OS that the model will run on after the compilation job has
-- completed.
--
-- 'compilationJobName', 'compilationJobSummary_compilationJobName' - The name of the model compilation job that you want a summary for.
--
-- 'compilationJobArn', 'compilationJobSummary_compilationJobArn' - The Amazon Resource Name (ARN) of the model compilation job.
--
-- 'creationTime', 'compilationJobSummary_creationTime' - The time when the model compilation job was created.
--
-- 'compilationJobStatus', 'compilationJobSummary_compilationJobStatus' - The status of the model compilation job.
newCompilationJobSummary ::
  -- | 'compilationJobName'
  Prelude.Text ->
  -- | 'compilationJobArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'compilationJobStatus'
  CompilationJobStatus ->
  CompilationJobSummary
newCompilationJobSummary
  pCompilationJobName_
  pCompilationJobArn_
  pCreationTime_
  pCompilationJobStatus_ =
    CompilationJobSummary'
      { compilationStartTime =
          Prelude.Nothing,
        compilationTargetPlatformAccelerator =
          Prelude.Nothing,
        compilationTargetDevice = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        compilationTargetPlatformArch = Prelude.Nothing,
        compilationEndTime = Prelude.Nothing,
        compilationTargetPlatformOs = Prelude.Nothing,
        compilationJobName = pCompilationJobName_,
        compilationJobArn = pCompilationJobArn_,
        creationTime = Core._Time Lens.# pCreationTime_,
        compilationJobStatus = pCompilationJobStatus_
      }

-- | The time when the model compilation job started.
compilationJobSummary_compilationStartTime :: Lens.Lens' CompilationJobSummary (Prelude.Maybe Prelude.UTCTime)
compilationJobSummary_compilationStartTime = Lens.lens (\CompilationJobSummary' {compilationStartTime} -> compilationStartTime) (\s@CompilationJobSummary' {} a -> s {compilationStartTime = a} :: CompilationJobSummary) Prelude.. Lens.mapping Core._Time

-- | The type of accelerator that the model will run on after the compilation
-- job has completed.
compilationJobSummary_compilationTargetPlatformAccelerator :: Lens.Lens' CompilationJobSummary (Prelude.Maybe TargetPlatformAccelerator)
compilationJobSummary_compilationTargetPlatformAccelerator = Lens.lens (\CompilationJobSummary' {compilationTargetPlatformAccelerator} -> compilationTargetPlatformAccelerator) (\s@CompilationJobSummary' {} a -> s {compilationTargetPlatformAccelerator = a} :: CompilationJobSummary)

-- | The type of device that the model will run on after the compilation job
-- has completed.
compilationJobSummary_compilationTargetDevice :: Lens.Lens' CompilationJobSummary (Prelude.Maybe TargetDevice)
compilationJobSummary_compilationTargetDevice = Lens.lens (\CompilationJobSummary' {compilationTargetDevice} -> compilationTargetDevice) (\s@CompilationJobSummary' {} a -> s {compilationTargetDevice = a} :: CompilationJobSummary)

-- | The time when the model compilation job was last modified.
compilationJobSummary_lastModifiedTime :: Lens.Lens' CompilationJobSummary (Prelude.Maybe Prelude.UTCTime)
compilationJobSummary_lastModifiedTime = Lens.lens (\CompilationJobSummary' {lastModifiedTime} -> lastModifiedTime) (\s@CompilationJobSummary' {} a -> s {lastModifiedTime = a} :: CompilationJobSummary) Prelude.. Lens.mapping Core._Time

-- | The type of architecture that the model will run on after the
-- compilation job has completed.
compilationJobSummary_compilationTargetPlatformArch :: Lens.Lens' CompilationJobSummary (Prelude.Maybe TargetPlatformArch)
compilationJobSummary_compilationTargetPlatformArch = Lens.lens (\CompilationJobSummary' {compilationTargetPlatformArch} -> compilationTargetPlatformArch) (\s@CompilationJobSummary' {} a -> s {compilationTargetPlatformArch = a} :: CompilationJobSummary)

-- | The time when the model compilation job completed.
compilationJobSummary_compilationEndTime :: Lens.Lens' CompilationJobSummary (Prelude.Maybe Prelude.UTCTime)
compilationJobSummary_compilationEndTime = Lens.lens (\CompilationJobSummary' {compilationEndTime} -> compilationEndTime) (\s@CompilationJobSummary' {} a -> s {compilationEndTime = a} :: CompilationJobSummary) Prelude.. Lens.mapping Core._Time

-- | The type of OS that the model will run on after the compilation job has
-- completed.
compilationJobSummary_compilationTargetPlatformOs :: Lens.Lens' CompilationJobSummary (Prelude.Maybe TargetPlatformOs)
compilationJobSummary_compilationTargetPlatformOs = Lens.lens (\CompilationJobSummary' {compilationTargetPlatformOs} -> compilationTargetPlatformOs) (\s@CompilationJobSummary' {} a -> s {compilationTargetPlatformOs = a} :: CompilationJobSummary)

-- | The name of the model compilation job that you want a summary for.
compilationJobSummary_compilationJobName :: Lens.Lens' CompilationJobSummary Prelude.Text
compilationJobSummary_compilationJobName = Lens.lens (\CompilationJobSummary' {compilationJobName} -> compilationJobName) (\s@CompilationJobSummary' {} a -> s {compilationJobName = a} :: CompilationJobSummary)

-- | The Amazon Resource Name (ARN) of the model compilation job.
compilationJobSummary_compilationJobArn :: Lens.Lens' CompilationJobSummary Prelude.Text
compilationJobSummary_compilationJobArn = Lens.lens (\CompilationJobSummary' {compilationJobArn} -> compilationJobArn) (\s@CompilationJobSummary' {} a -> s {compilationJobArn = a} :: CompilationJobSummary)

-- | The time when the model compilation job was created.
compilationJobSummary_creationTime :: Lens.Lens' CompilationJobSummary Prelude.UTCTime
compilationJobSummary_creationTime = Lens.lens (\CompilationJobSummary' {creationTime} -> creationTime) (\s@CompilationJobSummary' {} a -> s {creationTime = a} :: CompilationJobSummary) Prelude.. Core._Time

-- | The status of the model compilation job.
compilationJobSummary_compilationJobStatus :: Lens.Lens' CompilationJobSummary CompilationJobStatus
compilationJobSummary_compilationJobStatus = Lens.lens (\CompilationJobSummary' {compilationJobStatus} -> compilationJobStatus) (\s@CompilationJobSummary' {} a -> s {compilationJobStatus = a} :: CompilationJobSummary)

instance Core.FromJSON CompilationJobSummary where
  parseJSON =
    Core.withObject
      "CompilationJobSummary"
      ( \x ->
          CompilationJobSummary'
            Prelude.<$> (x Core..:? "CompilationStartTime")
            Prelude.<*> (x Core..:? "CompilationTargetPlatformAccelerator")
            Prelude.<*> (x Core..:? "CompilationTargetDevice")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "CompilationTargetPlatformArch")
            Prelude.<*> (x Core..:? "CompilationEndTime")
            Prelude.<*> (x Core..:? "CompilationTargetPlatformOs")
            Prelude.<*> (x Core..: "CompilationJobName")
            Prelude.<*> (x Core..: "CompilationJobArn")
            Prelude.<*> (x Core..: "CreationTime")
            Prelude.<*> (x Core..: "CompilationJobStatus")
      )

instance Prelude.Hashable CompilationJobSummary where
  hashWithSalt salt' CompilationJobSummary' {..} =
    salt' `Prelude.hashWithSalt` compilationJobStatus
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` compilationJobArn
      `Prelude.hashWithSalt` compilationJobName
      `Prelude.hashWithSalt` compilationTargetPlatformOs
      `Prelude.hashWithSalt` compilationEndTime
      `Prelude.hashWithSalt` compilationTargetPlatformArch
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` compilationTargetDevice
      `Prelude.hashWithSalt` compilationTargetPlatformAccelerator
      `Prelude.hashWithSalt` compilationStartTime

instance Prelude.NFData CompilationJobSummary where
  rnf CompilationJobSummary' {..} =
    Prelude.rnf compilationStartTime
      `Prelude.seq` Prelude.rnf compilationJobStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf compilationJobArn
      `Prelude.seq` Prelude.rnf compilationJobName
      `Prelude.seq` Prelude.rnf compilationTargetPlatformOs
      `Prelude.seq` Prelude.rnf compilationEndTime
      `Prelude.seq` Prelude.rnf compilationTargetPlatformArch
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf compilationTargetDevice
      `Prelude.seq` Prelude.rnf compilationTargetPlatformAccelerator
