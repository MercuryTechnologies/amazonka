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
-- Module      : Amazonka.QLDBSession.Types.StartSessionResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types.StartSessionResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDBSession.Types.TimingInformation

-- | Contains the details of the started session.
--
-- /See:/ 'newStartSessionResult' smart constructor.
data StartSessionResult = StartSessionResult'
  { -- | Contains server-side performance information for the command.
    timingInformation :: Prelude.Maybe TimingInformation,
    -- | Session token of the started session. This @SessionToken@ is required
    -- for every subsequent command that is issued during the current session.
    sessionToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartSessionResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timingInformation', 'startSessionResult_timingInformation' - Contains server-side performance information for the command.
--
-- 'sessionToken', 'startSessionResult_sessionToken' - Session token of the started session. This @SessionToken@ is required
-- for every subsequent command that is issued during the current session.
newStartSessionResult ::
  StartSessionResult
newStartSessionResult =
  StartSessionResult'
    { timingInformation =
        Prelude.Nothing,
      sessionToken = Prelude.Nothing
    }

-- | Contains server-side performance information for the command.
startSessionResult_timingInformation :: Lens.Lens' StartSessionResult (Prelude.Maybe TimingInformation)
startSessionResult_timingInformation = Lens.lens (\StartSessionResult' {timingInformation} -> timingInformation) (\s@StartSessionResult' {} a -> s {timingInformation = a} :: StartSessionResult)

-- | Session token of the started session. This @SessionToken@ is required
-- for every subsequent command that is issued during the current session.
startSessionResult_sessionToken :: Lens.Lens' StartSessionResult (Prelude.Maybe Prelude.Text)
startSessionResult_sessionToken = Lens.lens (\StartSessionResult' {sessionToken} -> sessionToken) (\s@StartSessionResult' {} a -> s {sessionToken = a} :: StartSessionResult)

instance Core.FromJSON StartSessionResult where
  parseJSON =
    Core.withObject
      "StartSessionResult"
      ( \x ->
          StartSessionResult'
            Prelude.<$> (x Core..:? "TimingInformation")
            Prelude.<*> (x Core..:? "SessionToken")
      )

instance Prelude.Hashable StartSessionResult where
  hashWithSalt salt' StartSessionResult' {..} =
    salt' `Prelude.hashWithSalt` sessionToken
      `Prelude.hashWithSalt` timingInformation

instance Prelude.NFData StartSessionResult where
  rnf StartSessionResult' {..} =
    Prelude.rnf timingInformation
      `Prelude.seq` Prelude.rnf sessionToken
