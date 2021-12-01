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
-- Module      : Amazonka.SageMaker.Types.TrialComponentSourceDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrialComponentSourceDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProcessingJob
import Amazonka.SageMaker.Types.TrainingJob
import Amazonka.SageMaker.Types.TransformJob

-- | Detailed information about the source of a trial component. Either
-- @ProcessingJob@ or @TrainingJob@ is returned.
--
-- /See:/ 'newTrialComponentSourceDetail' smart constructor.
data TrialComponentSourceDetail = TrialComponentSourceDetail'
  { -- | Information about a training job that\'s the source of a trial
    -- component.
    trainingJob :: Prelude.Maybe TrainingJob,
    -- | The Amazon Resource Name (ARN) of the source.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | Information about a processing job that\'s the source of a trial
    -- component.
    processingJob :: Prelude.Maybe ProcessingJob,
    -- | Information about a transform job that\'s the source of a trial
    -- component.
    transformJob :: Prelude.Maybe TransformJob
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrialComponentSourceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trainingJob', 'trialComponentSourceDetail_trainingJob' - Information about a training job that\'s the source of a trial
-- component.
--
-- 'sourceArn', 'trialComponentSourceDetail_sourceArn' - The Amazon Resource Name (ARN) of the source.
--
-- 'processingJob', 'trialComponentSourceDetail_processingJob' - Information about a processing job that\'s the source of a trial
-- component.
--
-- 'transformJob', 'trialComponentSourceDetail_transformJob' - Information about a transform job that\'s the source of a trial
-- component.
newTrialComponentSourceDetail ::
  TrialComponentSourceDetail
newTrialComponentSourceDetail =
  TrialComponentSourceDetail'
    { trainingJob =
        Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      processingJob = Prelude.Nothing,
      transformJob = Prelude.Nothing
    }

-- | Information about a training job that\'s the source of a trial
-- component.
trialComponentSourceDetail_trainingJob :: Lens.Lens' TrialComponentSourceDetail (Prelude.Maybe TrainingJob)
trialComponentSourceDetail_trainingJob = Lens.lens (\TrialComponentSourceDetail' {trainingJob} -> trainingJob) (\s@TrialComponentSourceDetail' {} a -> s {trainingJob = a} :: TrialComponentSourceDetail)

-- | The Amazon Resource Name (ARN) of the source.
trialComponentSourceDetail_sourceArn :: Lens.Lens' TrialComponentSourceDetail (Prelude.Maybe Prelude.Text)
trialComponentSourceDetail_sourceArn = Lens.lens (\TrialComponentSourceDetail' {sourceArn} -> sourceArn) (\s@TrialComponentSourceDetail' {} a -> s {sourceArn = a} :: TrialComponentSourceDetail)

-- | Information about a processing job that\'s the source of a trial
-- component.
trialComponentSourceDetail_processingJob :: Lens.Lens' TrialComponentSourceDetail (Prelude.Maybe ProcessingJob)
trialComponentSourceDetail_processingJob = Lens.lens (\TrialComponentSourceDetail' {processingJob} -> processingJob) (\s@TrialComponentSourceDetail' {} a -> s {processingJob = a} :: TrialComponentSourceDetail)

-- | Information about a transform job that\'s the source of a trial
-- component.
trialComponentSourceDetail_transformJob :: Lens.Lens' TrialComponentSourceDetail (Prelude.Maybe TransformJob)
trialComponentSourceDetail_transformJob = Lens.lens (\TrialComponentSourceDetail' {transformJob} -> transformJob) (\s@TrialComponentSourceDetail' {} a -> s {transformJob = a} :: TrialComponentSourceDetail)

instance Core.FromJSON TrialComponentSourceDetail where
  parseJSON =
    Core.withObject
      "TrialComponentSourceDetail"
      ( \x ->
          TrialComponentSourceDetail'
            Prelude.<$> (x Core..:? "TrainingJob")
            Prelude.<*> (x Core..:? "SourceArn")
            Prelude.<*> (x Core..:? "ProcessingJob")
            Prelude.<*> (x Core..:? "TransformJob")
      )

instance Prelude.Hashable TrialComponentSourceDetail where
  hashWithSalt salt' TrialComponentSourceDetail' {..} =
    salt' `Prelude.hashWithSalt` transformJob
      `Prelude.hashWithSalt` processingJob
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` trainingJob

instance Prelude.NFData TrialComponentSourceDetail where
  rnf TrialComponentSourceDetail' {..} =
    Prelude.rnf trainingJob
      `Prelude.seq` Prelude.rnf transformJob
      `Prelude.seq` Prelude.rnf processingJob
      `Prelude.seq` Prelude.rnf sourceArn
