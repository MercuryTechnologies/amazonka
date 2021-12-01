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
-- Module      : Amazonka.AppMesh.Types.HttpRetryPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpRetryPolicy where

import Amazonka.AppMesh.Types.Duration
import Amazonka.AppMesh.Types.TcpRetryPolicyEvent
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a retry policy. Specify at least one value for
-- at least one of the types of @RetryEvents@, a value for @maxRetries@,
-- and a value for @perRetryTimeout@. Both @server-error@ and
-- @gateway-error@ under @httpRetryEvents@ include the Envoy @reset@
-- policy. For more information on the @reset@ policy, see the
-- <https://www.envoyproxy.io/docs/envoy/latest/configuration/http/http_filters/router_filter#x-envoy-retry-on Envoy documentation>.
--
-- /See:/ 'newHttpRetryPolicy' smart constructor.
data HttpRetryPolicy = HttpRetryPolicy'
  { -- | Specify at least one of the following values.
    --
    -- -   __server-error__ – HTTP status codes 500, 501, 502, 503, 504, 505,
    --     506, 507, 508, 510, and 511
    --
    -- -   __gateway-error__ – HTTP status codes 502, 503, and 504
    --
    -- -   __client-error__ – HTTP status code 409
    --
    -- -   __stream-error__ – Retry on refused stream
    httpRetryEvents :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Specify a valid value. The event occurs before any processing of a
    -- request has started and is encountered when the upstream is temporarily
    -- or permanently unavailable.
    tcpRetryEvents :: Prelude.Maybe (Prelude.NonEmpty TcpRetryPolicyEvent),
    -- | The maximum number of retry attempts.
    maxRetries :: Prelude.Natural,
    -- | The timeout for each retry attempt.
    perRetryTimeout :: Duration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpRetryPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpRetryEvents', 'httpRetryPolicy_httpRetryEvents' - Specify at least one of the following values.
--
-- -   __server-error__ – HTTP status codes 500, 501, 502, 503, 504, 505,
--     506, 507, 508, 510, and 511
--
-- -   __gateway-error__ – HTTP status codes 502, 503, and 504
--
-- -   __client-error__ – HTTP status code 409
--
-- -   __stream-error__ – Retry on refused stream
--
-- 'tcpRetryEvents', 'httpRetryPolicy_tcpRetryEvents' - Specify a valid value. The event occurs before any processing of a
-- request has started and is encountered when the upstream is temporarily
-- or permanently unavailable.
--
-- 'maxRetries', 'httpRetryPolicy_maxRetries' - The maximum number of retry attempts.
--
-- 'perRetryTimeout', 'httpRetryPolicy_perRetryTimeout' - The timeout for each retry attempt.
newHttpRetryPolicy ::
  -- | 'maxRetries'
  Prelude.Natural ->
  -- | 'perRetryTimeout'
  Duration ->
  HttpRetryPolicy
newHttpRetryPolicy pMaxRetries_ pPerRetryTimeout_ =
  HttpRetryPolicy'
    { httpRetryEvents = Prelude.Nothing,
      tcpRetryEvents = Prelude.Nothing,
      maxRetries = pMaxRetries_,
      perRetryTimeout = pPerRetryTimeout_
    }

-- | Specify at least one of the following values.
--
-- -   __server-error__ – HTTP status codes 500, 501, 502, 503, 504, 505,
--     506, 507, 508, 510, and 511
--
-- -   __gateway-error__ – HTTP status codes 502, 503, and 504
--
-- -   __client-error__ – HTTP status code 409
--
-- -   __stream-error__ – Retry on refused stream
httpRetryPolicy_httpRetryEvents :: Lens.Lens' HttpRetryPolicy (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
httpRetryPolicy_httpRetryEvents = Lens.lens (\HttpRetryPolicy' {httpRetryEvents} -> httpRetryEvents) (\s@HttpRetryPolicy' {} a -> s {httpRetryEvents = a} :: HttpRetryPolicy) Prelude.. Lens.mapping Lens.coerced

-- | Specify a valid value. The event occurs before any processing of a
-- request has started and is encountered when the upstream is temporarily
-- or permanently unavailable.
httpRetryPolicy_tcpRetryEvents :: Lens.Lens' HttpRetryPolicy (Prelude.Maybe (Prelude.NonEmpty TcpRetryPolicyEvent))
httpRetryPolicy_tcpRetryEvents = Lens.lens (\HttpRetryPolicy' {tcpRetryEvents} -> tcpRetryEvents) (\s@HttpRetryPolicy' {} a -> s {tcpRetryEvents = a} :: HttpRetryPolicy) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of retry attempts.
httpRetryPolicy_maxRetries :: Lens.Lens' HttpRetryPolicy Prelude.Natural
httpRetryPolicy_maxRetries = Lens.lens (\HttpRetryPolicy' {maxRetries} -> maxRetries) (\s@HttpRetryPolicy' {} a -> s {maxRetries = a} :: HttpRetryPolicy)

-- | The timeout for each retry attempt.
httpRetryPolicy_perRetryTimeout :: Lens.Lens' HttpRetryPolicy Duration
httpRetryPolicy_perRetryTimeout = Lens.lens (\HttpRetryPolicy' {perRetryTimeout} -> perRetryTimeout) (\s@HttpRetryPolicy' {} a -> s {perRetryTimeout = a} :: HttpRetryPolicy)

instance Core.FromJSON HttpRetryPolicy where
  parseJSON =
    Core.withObject
      "HttpRetryPolicy"
      ( \x ->
          HttpRetryPolicy'
            Prelude.<$> (x Core..:? "httpRetryEvents")
            Prelude.<*> (x Core..:? "tcpRetryEvents")
            Prelude.<*> (x Core..: "maxRetries")
            Prelude.<*> (x Core..: "perRetryTimeout")
      )

instance Prelude.Hashable HttpRetryPolicy where
  hashWithSalt salt' HttpRetryPolicy' {..} =
    salt' `Prelude.hashWithSalt` perRetryTimeout
      `Prelude.hashWithSalt` maxRetries
      `Prelude.hashWithSalt` tcpRetryEvents
      `Prelude.hashWithSalt` httpRetryEvents

instance Prelude.NFData HttpRetryPolicy where
  rnf HttpRetryPolicy' {..} =
    Prelude.rnf httpRetryEvents
      `Prelude.seq` Prelude.rnf perRetryTimeout
      `Prelude.seq` Prelude.rnf maxRetries
      `Prelude.seq` Prelude.rnf tcpRetryEvents

instance Core.ToJSON HttpRetryPolicy where
  toJSON HttpRetryPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("httpRetryEvents" Core..=)
              Prelude.<$> httpRetryEvents,
            ("tcpRetryEvents" Core..=)
              Prelude.<$> tcpRetryEvents,
            Prelude.Just ("maxRetries" Core..= maxRetries),
            Prelude.Just
              ("perRetryTimeout" Core..= perRetryTimeout)
          ]
      )
