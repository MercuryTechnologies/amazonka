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
-- Module      : Amazonka.SageMaker.Types.ContextSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ContextSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ContextSource

-- | Lists a summary of the properties of a context. A context provides a
-- logical grouping of other entities.
--
-- /See:/ 'newContextSummary' smart constructor.
data ContextSummary = ContextSummary'
  { -- | When the context was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | When the context was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The type of the context.
    contextType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the context.
    contextArn :: Prelude.Maybe Prelude.Text,
    -- | The source of the context.
    source :: Prelude.Maybe ContextSource,
    -- | The name of the context.
    contextName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContextSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'contextSummary_creationTime' - When the context was created.
--
-- 'lastModifiedTime', 'contextSummary_lastModifiedTime' - When the context was last modified.
--
-- 'contextType', 'contextSummary_contextType' - The type of the context.
--
-- 'contextArn', 'contextSummary_contextArn' - The Amazon Resource Name (ARN) of the context.
--
-- 'source', 'contextSummary_source' - The source of the context.
--
-- 'contextName', 'contextSummary_contextName' - The name of the context.
newContextSummary ::
  ContextSummary
newContextSummary =
  ContextSummary'
    { creationTime = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      contextType = Prelude.Nothing,
      contextArn = Prelude.Nothing,
      source = Prelude.Nothing,
      contextName = Prelude.Nothing
    }

-- | When the context was created.
contextSummary_creationTime :: Lens.Lens' ContextSummary (Prelude.Maybe Prelude.UTCTime)
contextSummary_creationTime = Lens.lens (\ContextSummary' {creationTime} -> creationTime) (\s@ContextSummary' {} a -> s {creationTime = a} :: ContextSummary) Prelude.. Lens.mapping Core._Time

-- | When the context was last modified.
contextSummary_lastModifiedTime :: Lens.Lens' ContextSummary (Prelude.Maybe Prelude.UTCTime)
contextSummary_lastModifiedTime = Lens.lens (\ContextSummary' {lastModifiedTime} -> lastModifiedTime) (\s@ContextSummary' {} a -> s {lastModifiedTime = a} :: ContextSummary) Prelude.. Lens.mapping Core._Time

-- | The type of the context.
contextSummary_contextType :: Lens.Lens' ContextSummary (Prelude.Maybe Prelude.Text)
contextSummary_contextType = Lens.lens (\ContextSummary' {contextType} -> contextType) (\s@ContextSummary' {} a -> s {contextType = a} :: ContextSummary)

-- | The Amazon Resource Name (ARN) of the context.
contextSummary_contextArn :: Lens.Lens' ContextSummary (Prelude.Maybe Prelude.Text)
contextSummary_contextArn = Lens.lens (\ContextSummary' {contextArn} -> contextArn) (\s@ContextSummary' {} a -> s {contextArn = a} :: ContextSummary)

-- | The source of the context.
contextSummary_source :: Lens.Lens' ContextSummary (Prelude.Maybe ContextSource)
contextSummary_source = Lens.lens (\ContextSummary' {source} -> source) (\s@ContextSummary' {} a -> s {source = a} :: ContextSummary)

-- | The name of the context.
contextSummary_contextName :: Lens.Lens' ContextSummary (Prelude.Maybe Prelude.Text)
contextSummary_contextName = Lens.lens (\ContextSummary' {contextName} -> contextName) (\s@ContextSummary' {} a -> s {contextName = a} :: ContextSummary)

instance Core.FromJSON ContextSummary where
  parseJSON =
    Core.withObject
      "ContextSummary"
      ( \x ->
          ContextSummary'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "ContextType")
            Prelude.<*> (x Core..:? "ContextArn")
            Prelude.<*> (x Core..:? "Source")
            Prelude.<*> (x Core..:? "ContextName")
      )

instance Prelude.Hashable ContextSummary where
  hashWithSalt salt' ContextSummary' {..} =
    salt' `Prelude.hashWithSalt` contextName
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` contextArn
      `Prelude.hashWithSalt` contextType
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData ContextSummary where
  rnf ContextSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf contextName
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf contextArn
      `Prelude.seq` Prelude.rnf contextType
      `Prelude.seq` Prelude.rnf lastModifiedTime
