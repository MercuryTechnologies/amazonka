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
-- Module      : Amazonka.CloudFront.Types.FunctionMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.FunctionMetadata where

import Amazonka.CloudFront.Types.FunctionStage
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains metadata about a CloudFront function.
--
-- /See:/ 'newFunctionMetadata' smart constructor.
data FunctionMetadata = FunctionMetadata'
  { -- | The stage that the function is in, either @DEVELOPMENT@ or @LIVE@.
    --
    -- When a function is in the @DEVELOPMENT@ stage, you can test the function
    -- with @TestFunction@, and update it with @UpdateFunction@.
    --
    -- When a function is in the @LIVE@ stage, you can attach the function to a
    -- distribution’s cache behavior, using the function’s ARN.
    stage :: Prelude.Maybe FunctionStage,
    -- | The date and time when the function was created.
    createdTime :: Prelude.Maybe Core.ISO8601,
    -- | The Amazon Resource Name (ARN) of the function. The ARN uniquely
    -- identifies the function.
    functionARN :: Prelude.Text,
    -- | The date and time when the function was most recently updated.
    lastModifiedTime :: Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stage', 'functionMetadata_stage' - The stage that the function is in, either @DEVELOPMENT@ or @LIVE@.
--
-- When a function is in the @DEVELOPMENT@ stage, you can test the function
-- with @TestFunction@, and update it with @UpdateFunction@.
--
-- When a function is in the @LIVE@ stage, you can attach the function to a
-- distribution’s cache behavior, using the function’s ARN.
--
-- 'createdTime', 'functionMetadata_createdTime' - The date and time when the function was created.
--
-- 'functionARN', 'functionMetadata_functionARN' - The Amazon Resource Name (ARN) of the function. The ARN uniquely
-- identifies the function.
--
-- 'lastModifiedTime', 'functionMetadata_lastModifiedTime' - The date and time when the function was most recently updated.
newFunctionMetadata ::
  -- | 'functionARN'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  FunctionMetadata
newFunctionMetadata pFunctionARN_ pLastModifiedTime_ =
  FunctionMetadata'
    { stage = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      functionARN = pFunctionARN_,
      lastModifiedTime =
        Core._Time Lens.# pLastModifiedTime_
    }

-- | The stage that the function is in, either @DEVELOPMENT@ or @LIVE@.
--
-- When a function is in the @DEVELOPMENT@ stage, you can test the function
-- with @TestFunction@, and update it with @UpdateFunction@.
--
-- When a function is in the @LIVE@ stage, you can attach the function to a
-- distribution’s cache behavior, using the function’s ARN.
functionMetadata_stage :: Lens.Lens' FunctionMetadata (Prelude.Maybe FunctionStage)
functionMetadata_stage = Lens.lens (\FunctionMetadata' {stage} -> stage) (\s@FunctionMetadata' {} a -> s {stage = a} :: FunctionMetadata)

-- | The date and time when the function was created.
functionMetadata_createdTime :: Lens.Lens' FunctionMetadata (Prelude.Maybe Prelude.UTCTime)
functionMetadata_createdTime = Lens.lens (\FunctionMetadata' {createdTime} -> createdTime) (\s@FunctionMetadata' {} a -> s {createdTime = a} :: FunctionMetadata) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the function. The ARN uniquely
-- identifies the function.
functionMetadata_functionARN :: Lens.Lens' FunctionMetadata Prelude.Text
functionMetadata_functionARN = Lens.lens (\FunctionMetadata' {functionARN} -> functionARN) (\s@FunctionMetadata' {} a -> s {functionARN = a} :: FunctionMetadata)

-- | The date and time when the function was most recently updated.
functionMetadata_lastModifiedTime :: Lens.Lens' FunctionMetadata Prelude.UTCTime
functionMetadata_lastModifiedTime = Lens.lens (\FunctionMetadata' {lastModifiedTime} -> lastModifiedTime) (\s@FunctionMetadata' {} a -> s {lastModifiedTime = a} :: FunctionMetadata) Prelude.. Core._Time

instance Core.FromXML FunctionMetadata where
  parseXML x =
    FunctionMetadata'
      Prelude.<$> (x Core..@? "Stage")
      Prelude.<*> (x Core..@? "CreatedTime")
      Prelude.<*> (x Core..@ "FunctionARN")
      Prelude.<*> (x Core..@ "LastModifiedTime")

instance Prelude.Hashable FunctionMetadata where
  hashWithSalt salt' FunctionMetadata' {..} =
    salt' `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` functionARN
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` stage

instance Prelude.NFData FunctionMetadata where
  rnf FunctionMetadata' {..} =
    Prelude.rnf stage
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf functionARN
      `Prelude.seq` Prelude.rnf createdTime
