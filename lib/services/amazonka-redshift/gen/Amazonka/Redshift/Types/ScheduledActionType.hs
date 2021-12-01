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
-- Module      : Amazonka.Redshift.Types.ScheduledActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ScheduledActionType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.PauseClusterMessage
import Amazonka.Redshift.Types.ResizeClusterMessage
import Amazonka.Redshift.Types.ResumeClusterMessage

-- | The action type that specifies an Amazon Redshift API operation that is
-- supported by the Amazon Redshift scheduler.
--
-- /See:/ 'newScheduledActionType' smart constructor.
data ScheduledActionType = ScheduledActionType'
  { -- | An action that runs a @ResizeCluster@ API operation.
    resizeCluster :: Prelude.Maybe ResizeClusterMessage,
    -- | An action that runs a @ResumeCluster@ API operation.
    resumeCluster :: Prelude.Maybe ResumeClusterMessage,
    -- | An action that runs a @PauseCluster@ API operation.
    pauseCluster :: Prelude.Maybe PauseClusterMessage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledActionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resizeCluster', 'scheduledActionType_resizeCluster' - An action that runs a @ResizeCluster@ API operation.
--
-- 'resumeCluster', 'scheduledActionType_resumeCluster' - An action that runs a @ResumeCluster@ API operation.
--
-- 'pauseCluster', 'scheduledActionType_pauseCluster' - An action that runs a @PauseCluster@ API operation.
newScheduledActionType ::
  ScheduledActionType
newScheduledActionType =
  ScheduledActionType'
    { resizeCluster =
        Prelude.Nothing,
      resumeCluster = Prelude.Nothing,
      pauseCluster = Prelude.Nothing
    }

-- | An action that runs a @ResizeCluster@ API operation.
scheduledActionType_resizeCluster :: Lens.Lens' ScheduledActionType (Prelude.Maybe ResizeClusterMessage)
scheduledActionType_resizeCluster = Lens.lens (\ScheduledActionType' {resizeCluster} -> resizeCluster) (\s@ScheduledActionType' {} a -> s {resizeCluster = a} :: ScheduledActionType)

-- | An action that runs a @ResumeCluster@ API operation.
scheduledActionType_resumeCluster :: Lens.Lens' ScheduledActionType (Prelude.Maybe ResumeClusterMessage)
scheduledActionType_resumeCluster = Lens.lens (\ScheduledActionType' {resumeCluster} -> resumeCluster) (\s@ScheduledActionType' {} a -> s {resumeCluster = a} :: ScheduledActionType)

-- | An action that runs a @PauseCluster@ API operation.
scheduledActionType_pauseCluster :: Lens.Lens' ScheduledActionType (Prelude.Maybe PauseClusterMessage)
scheduledActionType_pauseCluster = Lens.lens (\ScheduledActionType' {pauseCluster} -> pauseCluster) (\s@ScheduledActionType' {} a -> s {pauseCluster = a} :: ScheduledActionType)

instance Core.FromXML ScheduledActionType where
  parseXML x =
    ScheduledActionType'
      Prelude.<$> (x Core..@? "ResizeCluster")
      Prelude.<*> (x Core..@? "ResumeCluster")
      Prelude.<*> (x Core..@? "PauseCluster")

instance Prelude.Hashable ScheduledActionType where
  hashWithSalt salt' ScheduledActionType' {..} =
    salt' `Prelude.hashWithSalt` pauseCluster
      `Prelude.hashWithSalt` resumeCluster
      `Prelude.hashWithSalt` resizeCluster

instance Prelude.NFData ScheduledActionType where
  rnf ScheduledActionType' {..} =
    Prelude.rnf resizeCluster
      `Prelude.seq` Prelude.rnf pauseCluster
      `Prelude.seq` Prelude.rnf resumeCluster

instance Core.ToQuery ScheduledActionType where
  toQuery ScheduledActionType' {..} =
    Prelude.mconcat
      [ "ResizeCluster" Core.=: resizeCluster,
        "ResumeCluster" Core.=: resumeCluster,
        "PauseCluster" Core.=: pauseCluster
      ]
