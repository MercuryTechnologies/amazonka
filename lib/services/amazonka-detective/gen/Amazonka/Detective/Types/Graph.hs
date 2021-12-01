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
-- Module      : Amazonka.Detective.Types.Graph
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Types.Graph where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A behavior graph in Detective.
--
-- /See:/ 'newGraph' smart constructor.
data Graph = Graph'
  { -- | The ARN of the behavior graph.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the behavior graph was created. The value is in
    -- milliseconds since the epoch.
    createdTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Graph' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'graph_arn' - The ARN of the behavior graph.
--
-- 'createdTime', 'graph_createdTime' - The date and time that the behavior graph was created. The value is in
-- milliseconds since the epoch.
newGraph ::
  Graph
newGraph =
  Graph'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing
    }

-- | The ARN of the behavior graph.
graph_arn :: Lens.Lens' Graph (Prelude.Maybe Prelude.Text)
graph_arn = Lens.lens (\Graph' {arn} -> arn) (\s@Graph' {} a -> s {arn = a} :: Graph)

-- | The date and time that the behavior graph was created. The value is in
-- milliseconds since the epoch.
graph_createdTime :: Lens.Lens' Graph (Prelude.Maybe Prelude.UTCTime)
graph_createdTime = Lens.lens (\Graph' {createdTime} -> createdTime) (\s@Graph' {} a -> s {createdTime = a} :: Graph) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Graph where
  parseJSON =
    Core.withObject
      "Graph"
      ( \x ->
          Graph'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
      )

instance Prelude.Hashable Graph where
  hashWithSalt salt' Graph' {..} =
    salt' `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn

instance Prelude.NFData Graph where
  rnf Graph' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
