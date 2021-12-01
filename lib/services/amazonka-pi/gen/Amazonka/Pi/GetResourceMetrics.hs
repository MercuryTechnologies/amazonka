{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pi.GetResourceMetrics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve Performance Insights metrics for a set of data sources, over a
-- time period. You can provide specific dimension groups and dimensions,
-- and provide aggregation and filtering criteria for each group.
--
-- Each response element returns a maximum of 500 bytes. For larger
-- elements, such as SQL statements, only the first 500 bytes are returned.
module Amazonka.Pi.GetResourceMetrics
  ( -- * Creating a Request
    GetResourceMetrics (..),
    newGetResourceMetrics,

    -- * Request Lenses
    getResourceMetrics_periodInSeconds,
    getResourceMetrics_nextToken,
    getResourceMetrics_maxResults,
    getResourceMetrics_serviceType,
    getResourceMetrics_identifier,
    getResourceMetrics_metricQueries,
    getResourceMetrics_startTime,
    getResourceMetrics_endTime,

    -- * Destructuring the Response
    GetResourceMetricsResponse (..),
    newGetResourceMetricsResponse,

    -- * Response Lenses
    getResourceMetricsResponse_identifier,
    getResourceMetricsResponse_metricList,
    getResourceMetricsResponse_alignedEndTime,
    getResourceMetricsResponse_alignedStartTime,
    getResourceMetricsResponse_nextToken,
    getResourceMetricsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pi.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourceMetrics' smart constructor.
data GetResourceMetrics = GetResourceMetrics'
  { -- | The granularity, in seconds, of the data points returned from
    -- Performance Insights. A period can be as short as one second, or as long
    -- as one day (86400 seconds). Valid values are:
    --
    -- -   @1@ (one second)
    --
    -- -   @60@ (one minute)
    --
    -- -   @300@ (five minutes)
    --
    -- -   @3600@ (one hour)
    --
    -- -   @86400@ (twenty-four hours)
    --
    -- If you don\'t specify @PeriodInSeconds@, then Performance Insights will
    -- choose a value for you, with a goal of returning roughly 100-200 data
    -- points in the response.
    periodInSeconds :: Prelude.Maybe Prelude.Int,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- token, up to the value specified by @MaxRecords@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return in the response. If more items
    -- exist than the specified @MaxRecords@ value, a pagination token is
    -- included in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The AWS service for which Performance Insights returns metrics. The only
    -- valid value for /ServiceType/ is @RDS@.
    serviceType :: ServiceType,
    -- | An immutable, AWS Region-unique identifier for a data source.
    -- Performance Insights gathers metrics from this data source.
    --
    -- To use a DB instance as a data source, specify its @DbiResourceId@
    -- value. For example, specify @db-FAIHNTYBKTGAUSUZQYPDS2GW4A@.
    identifier :: Prelude.Text,
    -- | An array of one or more queries to perform. Each query must specify a
    -- Performance Insights metric, and can optionally specify aggregation and
    -- filtering criteria.
    metricQueries :: Prelude.NonEmpty MetricQuery,
    -- | The date and time specifying the beginning of the requested time series
    -- data. You can\'t specify a @StartTime@ that\'s earlier than 7 days ago.
    -- The value specified is /inclusive/ - data points equal to or greater
    -- than @StartTime@ will be returned.
    --
    -- The value for @StartTime@ must be earlier than the value for @EndTime@.
    startTime :: Core.POSIX,
    -- | The date and time specifying the end of the requested time series data.
    -- The value specified is /exclusive/ - data points less than (but not
    -- equal to) @EndTime@ will be returned.
    --
    -- The value for @EndTime@ must be later than the value for @StartTime@.
    endTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'periodInSeconds', 'getResourceMetrics_periodInSeconds' - The granularity, in seconds, of the data points returned from
-- Performance Insights. A period can be as short as one second, or as long
-- as one day (86400 seconds). Valid values are:
--
-- -   @1@ (one second)
--
-- -   @60@ (one minute)
--
-- -   @300@ (five minutes)
--
-- -   @3600@ (one hour)
--
-- -   @86400@ (twenty-four hours)
--
-- If you don\'t specify @PeriodInSeconds@, then Performance Insights will
-- choose a value for you, with a goal of returning roughly 100-200 data
-- points in the response.
--
-- 'nextToken', 'getResourceMetrics_nextToken' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- token, up to the value specified by @MaxRecords@.
--
-- 'maxResults', 'getResourceMetrics_maxResults' - The maximum number of items to return in the response. If more items
-- exist than the specified @MaxRecords@ value, a pagination token is
-- included in the response so that the remaining results can be retrieved.
--
-- 'serviceType', 'getResourceMetrics_serviceType' - The AWS service for which Performance Insights returns metrics. The only
-- valid value for /ServiceType/ is @RDS@.
--
-- 'identifier', 'getResourceMetrics_identifier' - An immutable, AWS Region-unique identifier for a data source.
-- Performance Insights gathers metrics from this data source.
--
-- To use a DB instance as a data source, specify its @DbiResourceId@
-- value. For example, specify @db-FAIHNTYBKTGAUSUZQYPDS2GW4A@.
--
-- 'metricQueries', 'getResourceMetrics_metricQueries' - An array of one or more queries to perform. Each query must specify a
-- Performance Insights metric, and can optionally specify aggregation and
-- filtering criteria.
--
-- 'startTime', 'getResourceMetrics_startTime' - The date and time specifying the beginning of the requested time series
-- data. You can\'t specify a @StartTime@ that\'s earlier than 7 days ago.
-- The value specified is /inclusive/ - data points equal to or greater
-- than @StartTime@ will be returned.
--
-- The value for @StartTime@ must be earlier than the value for @EndTime@.
--
-- 'endTime', 'getResourceMetrics_endTime' - The date and time specifying the end of the requested time series data.
-- The value specified is /exclusive/ - data points less than (but not
-- equal to) @EndTime@ will be returned.
--
-- The value for @EndTime@ must be later than the value for @StartTime@.
newGetResourceMetrics ::
  -- | 'serviceType'
  ServiceType ->
  -- | 'identifier'
  Prelude.Text ->
  -- | 'metricQueries'
  Prelude.NonEmpty MetricQuery ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  GetResourceMetrics
newGetResourceMetrics
  pServiceType_
  pIdentifier_
  pMetricQueries_
  pStartTime_
  pEndTime_ =
    GetResourceMetrics'
      { periodInSeconds =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        serviceType = pServiceType_,
        identifier = pIdentifier_,
        metricQueries = Lens.coerced Lens.# pMetricQueries_,
        startTime = Core._Time Lens.# pStartTime_,
        endTime = Core._Time Lens.# pEndTime_
      }

-- | The granularity, in seconds, of the data points returned from
-- Performance Insights. A period can be as short as one second, or as long
-- as one day (86400 seconds). Valid values are:
--
-- -   @1@ (one second)
--
-- -   @60@ (one minute)
--
-- -   @300@ (five minutes)
--
-- -   @3600@ (one hour)
--
-- -   @86400@ (twenty-four hours)
--
-- If you don\'t specify @PeriodInSeconds@, then Performance Insights will
-- choose a value for you, with a goal of returning roughly 100-200 data
-- points in the response.
getResourceMetrics_periodInSeconds :: Lens.Lens' GetResourceMetrics (Prelude.Maybe Prelude.Int)
getResourceMetrics_periodInSeconds = Lens.lens (\GetResourceMetrics' {periodInSeconds} -> periodInSeconds) (\s@GetResourceMetrics' {} a -> s {periodInSeconds = a} :: GetResourceMetrics)

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- token, up to the value specified by @MaxRecords@.
getResourceMetrics_nextToken :: Lens.Lens' GetResourceMetrics (Prelude.Maybe Prelude.Text)
getResourceMetrics_nextToken = Lens.lens (\GetResourceMetrics' {nextToken} -> nextToken) (\s@GetResourceMetrics' {} a -> s {nextToken = a} :: GetResourceMetrics)

-- | The maximum number of items to return in the response. If more items
-- exist than the specified @MaxRecords@ value, a pagination token is
-- included in the response so that the remaining results can be retrieved.
getResourceMetrics_maxResults :: Lens.Lens' GetResourceMetrics (Prelude.Maybe Prelude.Natural)
getResourceMetrics_maxResults = Lens.lens (\GetResourceMetrics' {maxResults} -> maxResults) (\s@GetResourceMetrics' {} a -> s {maxResults = a} :: GetResourceMetrics)

-- | The AWS service for which Performance Insights returns metrics. The only
-- valid value for /ServiceType/ is @RDS@.
getResourceMetrics_serviceType :: Lens.Lens' GetResourceMetrics ServiceType
getResourceMetrics_serviceType = Lens.lens (\GetResourceMetrics' {serviceType} -> serviceType) (\s@GetResourceMetrics' {} a -> s {serviceType = a} :: GetResourceMetrics)

-- | An immutable, AWS Region-unique identifier for a data source.
-- Performance Insights gathers metrics from this data source.
--
-- To use a DB instance as a data source, specify its @DbiResourceId@
-- value. For example, specify @db-FAIHNTYBKTGAUSUZQYPDS2GW4A@.
getResourceMetrics_identifier :: Lens.Lens' GetResourceMetrics Prelude.Text
getResourceMetrics_identifier = Lens.lens (\GetResourceMetrics' {identifier} -> identifier) (\s@GetResourceMetrics' {} a -> s {identifier = a} :: GetResourceMetrics)

-- | An array of one or more queries to perform. Each query must specify a
-- Performance Insights metric, and can optionally specify aggregation and
-- filtering criteria.
getResourceMetrics_metricQueries :: Lens.Lens' GetResourceMetrics (Prelude.NonEmpty MetricQuery)
getResourceMetrics_metricQueries = Lens.lens (\GetResourceMetrics' {metricQueries} -> metricQueries) (\s@GetResourceMetrics' {} a -> s {metricQueries = a} :: GetResourceMetrics) Prelude.. Lens.coerced

-- | The date and time specifying the beginning of the requested time series
-- data. You can\'t specify a @StartTime@ that\'s earlier than 7 days ago.
-- The value specified is /inclusive/ - data points equal to or greater
-- than @StartTime@ will be returned.
--
-- The value for @StartTime@ must be earlier than the value for @EndTime@.
getResourceMetrics_startTime :: Lens.Lens' GetResourceMetrics Prelude.UTCTime
getResourceMetrics_startTime = Lens.lens (\GetResourceMetrics' {startTime} -> startTime) (\s@GetResourceMetrics' {} a -> s {startTime = a} :: GetResourceMetrics) Prelude.. Core._Time

-- | The date and time specifying the end of the requested time series data.
-- The value specified is /exclusive/ - data points less than (but not
-- equal to) @EndTime@ will be returned.
--
-- The value for @EndTime@ must be later than the value for @StartTime@.
getResourceMetrics_endTime :: Lens.Lens' GetResourceMetrics Prelude.UTCTime
getResourceMetrics_endTime = Lens.lens (\GetResourceMetrics' {endTime} -> endTime) (\s@GetResourceMetrics' {} a -> s {endTime = a} :: GetResourceMetrics) Prelude.. Core._Time

instance Core.AWSRequest GetResourceMetrics where
  type
    AWSResponse GetResourceMetrics =
      GetResourceMetricsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceMetricsResponse'
            Prelude.<$> (x Core..?> "Identifier")
            Prelude.<*> (x Core..?> "MetricList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "AlignedEndTime")
            Prelude.<*> (x Core..?> "AlignedStartTime")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourceMetrics where
  hashWithSalt salt' GetResourceMetrics' {..} =
    salt' `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` metricQueries
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` serviceType
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` periodInSeconds

instance Prelude.NFData GetResourceMetrics where
  rnf GetResourceMetrics' {..} =
    Prelude.rnf periodInSeconds
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf metricQueries
      `Prelude.seq` Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf serviceType
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders GetResourceMetrics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PerformanceInsightsv20180227.GetResourceMetrics" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetResourceMetrics where
  toJSON GetResourceMetrics' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PeriodInSeconds" Core..=)
              Prelude.<$> periodInSeconds,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("ServiceType" Core..= serviceType),
            Prelude.Just ("Identifier" Core..= identifier),
            Prelude.Just ("MetricQueries" Core..= metricQueries),
            Prelude.Just ("StartTime" Core..= startTime),
            Prelude.Just ("EndTime" Core..= endTime)
          ]
      )

instance Core.ToPath GetResourceMetrics where
  toPath = Prelude.const "/"

instance Core.ToQuery GetResourceMetrics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceMetricsResponse' smart constructor.
data GetResourceMetricsResponse = GetResourceMetricsResponse'
  { -- | An immutable, AWS Region-unique identifier for a data source.
    -- Performance Insights gathers metrics from this data source.
    --
    -- To use a DB instance as a data source, you specify its @DbiResourceId@
    -- value - for example: @db-FAIHNTYBKTGAUSUZQYPDS2GW4A@
    identifier :: Prelude.Maybe Prelude.Text,
    -- | An array of metric results,, where each array element contains all of
    -- the data points for a particular dimension.
    metricList :: Prelude.Maybe [MetricKeyDataPoints],
    -- | The end time for the returned metrics, after alignment to a granular
    -- boundary (as specified by @PeriodInSeconds@). @AlignedEndTime@ will be
    -- greater than or equal to the value of the user-specified @Endtime@.
    alignedEndTime :: Prelude.Maybe Core.POSIX,
    -- | The start time for the returned metrics, after alignment to a granular
    -- boundary (as specified by @PeriodInSeconds@). @AlignedStartTime@ will be
    -- less than or equal to the value of the user-specified @StartTime@.
    alignedStartTime :: Prelude.Maybe Core.POSIX,
    -- | An optional pagination token provided by a previous request. If this
    -- parameter is specified, the response includes only records beyond the
    -- token, up to the value specified by @MaxRecords@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceMetricsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identifier', 'getResourceMetricsResponse_identifier' - An immutable, AWS Region-unique identifier for a data source.
-- Performance Insights gathers metrics from this data source.
--
-- To use a DB instance as a data source, you specify its @DbiResourceId@
-- value - for example: @db-FAIHNTYBKTGAUSUZQYPDS2GW4A@
--
-- 'metricList', 'getResourceMetricsResponse_metricList' - An array of metric results,, where each array element contains all of
-- the data points for a particular dimension.
--
-- 'alignedEndTime', 'getResourceMetricsResponse_alignedEndTime' - The end time for the returned metrics, after alignment to a granular
-- boundary (as specified by @PeriodInSeconds@). @AlignedEndTime@ will be
-- greater than or equal to the value of the user-specified @Endtime@.
--
-- 'alignedStartTime', 'getResourceMetricsResponse_alignedStartTime' - The start time for the returned metrics, after alignment to a granular
-- boundary (as specified by @PeriodInSeconds@). @AlignedStartTime@ will be
-- less than or equal to the value of the user-specified @StartTime@.
--
-- 'nextToken', 'getResourceMetricsResponse_nextToken' - An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- token, up to the value specified by @MaxRecords@.
--
-- 'httpStatus', 'getResourceMetricsResponse_httpStatus' - The response's http status code.
newGetResourceMetricsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceMetricsResponse
newGetResourceMetricsResponse pHttpStatus_ =
  GetResourceMetricsResponse'
    { identifier =
        Prelude.Nothing,
      metricList = Prelude.Nothing,
      alignedEndTime = Prelude.Nothing,
      alignedStartTime = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An immutable, AWS Region-unique identifier for a data source.
-- Performance Insights gathers metrics from this data source.
--
-- To use a DB instance as a data source, you specify its @DbiResourceId@
-- value - for example: @db-FAIHNTYBKTGAUSUZQYPDS2GW4A@
getResourceMetricsResponse_identifier :: Lens.Lens' GetResourceMetricsResponse (Prelude.Maybe Prelude.Text)
getResourceMetricsResponse_identifier = Lens.lens (\GetResourceMetricsResponse' {identifier} -> identifier) (\s@GetResourceMetricsResponse' {} a -> s {identifier = a} :: GetResourceMetricsResponse)

-- | An array of metric results,, where each array element contains all of
-- the data points for a particular dimension.
getResourceMetricsResponse_metricList :: Lens.Lens' GetResourceMetricsResponse (Prelude.Maybe [MetricKeyDataPoints])
getResourceMetricsResponse_metricList = Lens.lens (\GetResourceMetricsResponse' {metricList} -> metricList) (\s@GetResourceMetricsResponse' {} a -> s {metricList = a} :: GetResourceMetricsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The end time for the returned metrics, after alignment to a granular
-- boundary (as specified by @PeriodInSeconds@). @AlignedEndTime@ will be
-- greater than or equal to the value of the user-specified @Endtime@.
getResourceMetricsResponse_alignedEndTime :: Lens.Lens' GetResourceMetricsResponse (Prelude.Maybe Prelude.UTCTime)
getResourceMetricsResponse_alignedEndTime = Lens.lens (\GetResourceMetricsResponse' {alignedEndTime} -> alignedEndTime) (\s@GetResourceMetricsResponse' {} a -> s {alignedEndTime = a} :: GetResourceMetricsResponse) Prelude.. Lens.mapping Core._Time

-- | The start time for the returned metrics, after alignment to a granular
-- boundary (as specified by @PeriodInSeconds@). @AlignedStartTime@ will be
-- less than or equal to the value of the user-specified @StartTime@.
getResourceMetricsResponse_alignedStartTime :: Lens.Lens' GetResourceMetricsResponse (Prelude.Maybe Prelude.UTCTime)
getResourceMetricsResponse_alignedStartTime = Lens.lens (\GetResourceMetricsResponse' {alignedStartTime} -> alignedStartTime) (\s@GetResourceMetricsResponse' {} a -> s {alignedStartTime = a} :: GetResourceMetricsResponse) Prelude.. Lens.mapping Core._Time

-- | An optional pagination token provided by a previous request. If this
-- parameter is specified, the response includes only records beyond the
-- token, up to the value specified by @MaxRecords@.
getResourceMetricsResponse_nextToken :: Lens.Lens' GetResourceMetricsResponse (Prelude.Maybe Prelude.Text)
getResourceMetricsResponse_nextToken = Lens.lens (\GetResourceMetricsResponse' {nextToken} -> nextToken) (\s@GetResourceMetricsResponse' {} a -> s {nextToken = a} :: GetResourceMetricsResponse)

-- | The response's http status code.
getResourceMetricsResponse_httpStatus :: Lens.Lens' GetResourceMetricsResponse Prelude.Int
getResourceMetricsResponse_httpStatus = Lens.lens (\GetResourceMetricsResponse' {httpStatus} -> httpStatus) (\s@GetResourceMetricsResponse' {} a -> s {httpStatus = a} :: GetResourceMetricsResponse)

instance Prelude.NFData GetResourceMetricsResponse where
  rnf GetResourceMetricsResponse' {..} =
    Prelude.rnf identifier
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf alignedStartTime
      `Prelude.seq` Prelude.rnf alignedEndTime
      `Prelude.seq` Prelude.rnf metricList
