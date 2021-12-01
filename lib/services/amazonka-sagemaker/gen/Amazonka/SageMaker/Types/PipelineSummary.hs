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
-- Module      : Amazonka.SageMaker.Types.PipelineSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.PipelineSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A summary of a pipeline.
--
-- /See:/ 'newPipelineSummary' smart constructor.
data PipelineSummary = PipelineSummary'
  { -- | The creation time of the pipeline.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The display name of the pipeline.
    pipelineDisplayName :: Prelude.Maybe Prelude.Text,
    -- | The last time that a pipeline execution began.
    lastExecutionTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the pipeline.
    pipelineName :: Prelude.Maybe Prelude.Text,
    -- | The time that the pipeline was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The description of the pipeline.
    pipelineDescription :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the pipeline.
    pipelineArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that the pipeline used to execute.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'pipelineSummary_creationTime' - The creation time of the pipeline.
--
-- 'pipelineDisplayName', 'pipelineSummary_pipelineDisplayName' - The display name of the pipeline.
--
-- 'lastExecutionTime', 'pipelineSummary_lastExecutionTime' - The last time that a pipeline execution began.
--
-- 'pipelineName', 'pipelineSummary_pipelineName' - The name of the pipeline.
--
-- 'lastModifiedTime', 'pipelineSummary_lastModifiedTime' - The time that the pipeline was last modified.
--
-- 'pipelineDescription', 'pipelineSummary_pipelineDescription' - The description of the pipeline.
--
-- 'pipelineArn', 'pipelineSummary_pipelineArn' - The Amazon Resource Name (ARN) of the pipeline.
--
-- 'roleArn', 'pipelineSummary_roleArn' - The Amazon Resource Name (ARN) that the pipeline used to execute.
newPipelineSummary ::
  PipelineSummary
newPipelineSummary =
  PipelineSummary'
    { creationTime = Prelude.Nothing,
      pipelineDisplayName = Prelude.Nothing,
      lastExecutionTime = Prelude.Nothing,
      pipelineName = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      pipelineDescription = Prelude.Nothing,
      pipelineArn = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The creation time of the pipeline.
pipelineSummary_creationTime :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.UTCTime)
pipelineSummary_creationTime = Lens.lens (\PipelineSummary' {creationTime} -> creationTime) (\s@PipelineSummary' {} a -> s {creationTime = a} :: PipelineSummary) Prelude.. Lens.mapping Core._Time

-- | The display name of the pipeline.
pipelineSummary_pipelineDisplayName :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Text)
pipelineSummary_pipelineDisplayName = Lens.lens (\PipelineSummary' {pipelineDisplayName} -> pipelineDisplayName) (\s@PipelineSummary' {} a -> s {pipelineDisplayName = a} :: PipelineSummary)

-- | The last time that a pipeline execution began.
pipelineSummary_lastExecutionTime :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.UTCTime)
pipelineSummary_lastExecutionTime = Lens.lens (\PipelineSummary' {lastExecutionTime} -> lastExecutionTime) (\s@PipelineSummary' {} a -> s {lastExecutionTime = a} :: PipelineSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the pipeline.
pipelineSummary_pipelineName :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Text)
pipelineSummary_pipelineName = Lens.lens (\PipelineSummary' {pipelineName} -> pipelineName) (\s@PipelineSummary' {} a -> s {pipelineName = a} :: PipelineSummary)

-- | The time that the pipeline was last modified.
pipelineSummary_lastModifiedTime :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.UTCTime)
pipelineSummary_lastModifiedTime = Lens.lens (\PipelineSummary' {lastModifiedTime} -> lastModifiedTime) (\s@PipelineSummary' {} a -> s {lastModifiedTime = a} :: PipelineSummary) Prelude.. Lens.mapping Core._Time

-- | The description of the pipeline.
pipelineSummary_pipelineDescription :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Text)
pipelineSummary_pipelineDescription = Lens.lens (\PipelineSummary' {pipelineDescription} -> pipelineDescription) (\s@PipelineSummary' {} a -> s {pipelineDescription = a} :: PipelineSummary)

-- | The Amazon Resource Name (ARN) of the pipeline.
pipelineSummary_pipelineArn :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Text)
pipelineSummary_pipelineArn = Lens.lens (\PipelineSummary' {pipelineArn} -> pipelineArn) (\s@PipelineSummary' {} a -> s {pipelineArn = a} :: PipelineSummary)

-- | The Amazon Resource Name (ARN) that the pipeline used to execute.
pipelineSummary_roleArn :: Lens.Lens' PipelineSummary (Prelude.Maybe Prelude.Text)
pipelineSummary_roleArn = Lens.lens (\PipelineSummary' {roleArn} -> roleArn) (\s@PipelineSummary' {} a -> s {roleArn = a} :: PipelineSummary)

instance Core.FromJSON PipelineSummary where
  parseJSON =
    Core.withObject
      "PipelineSummary"
      ( \x ->
          PipelineSummary'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "PipelineDisplayName")
            Prelude.<*> (x Core..:? "LastExecutionTime")
            Prelude.<*> (x Core..:? "PipelineName")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "PipelineDescription")
            Prelude.<*> (x Core..:? "PipelineArn")
            Prelude.<*> (x Core..:? "RoleArn")
      )

instance Prelude.Hashable PipelineSummary where
  hashWithSalt salt' PipelineSummary' {..} =
    salt' `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` pipelineArn
      `Prelude.hashWithSalt` pipelineDescription
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` pipelineName
      `Prelude.hashWithSalt` lastExecutionTime
      `Prelude.hashWithSalt` pipelineDisplayName
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData PipelineSummary where
  rnf PipelineSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf pipelineArn
      `Prelude.seq` Prelude.rnf pipelineDescription
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf pipelineName
      `Prelude.seq` Prelude.rnf lastExecutionTime
      `Prelude.seq` Prelude.rnf pipelineDisplayName
